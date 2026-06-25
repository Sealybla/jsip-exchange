open! Core
open! Async
open Jsip_types
open Jsip_order_book

module Connection_state = struct
  type t = { mutable session : Session.t option }

  let participant t = Option.map t.session ~f:Session.participant
end

type t =
  { engine : Matching_engine.t
  ; dispatcher : Dispatcher.t
  ; request_writer : Order.Request.t Pipe.Writer.t
  ; tcp_server : (Socket.Address.Inet.t, int) Tcp.Server.t
  ; port : int
  }

(* Bound how many client requests can sit in the queue waiting for the
   matching engine. Once the queue is full, [Pipe.write] returns a pending
   deferred and the [submit_order_rpc] handler blocks until the engine has
   processed enough requests to free up space — clients get backpressure
   without the server's memory growing unboundedly. *)
let request_queue_size_budget = 1024

let handle_submit
  ~request_writer
  (request : Order.Request.t)
  (con_state : Connection_state.t)
  : unit Or_error.t Deferred.t
  =
  match con_state.session with
  | None -> return (Or_error.error_string [%string "not logged in"])
  | Some _ ->
    let new_req =
      { request with
        participant =
          Option.value_exn (Connection_state.participant con_state)
      }
    in
    let%bind () = Pipe.write_if_open request_writer new_req in
    return (Ok ())
;;

let start_matching_loop ~engine ~dispatcher request_reader =
  don't_wait_for
    (Pipe.iter_without_pushback request_reader ~f:(fun request ->
       let events = Matching_engine.submit engine request in
       Dispatcher.dispatch dispatcher events))
;;

let handle_login
  ~dispatcher
  (participant_name : string)
  (con_state : Connection_state.t)
  : Participant.t Or_error.t
  =
  let is_whitespace =
    String.for_all participant_name ~f:Char.is_whitespace
  in
  match is_whitespace with
  | true -> Or_error.error_string [%string "participant name is empty"]
  | false ->
    let participant = Participant.of_string participant_name in
    let set_up_success =
      Dispatcher.set_up_session_err dispatcher participant
    in
    (match set_up_success with
     (* store session in connection state so subsequence RPCs on same
        connection can find, return Ok(participant) *)
     | Ok _ ->
       let new_sess = Session.create participant in
       con_state.session <- Some new_sess;
       Ok participant
     | Error _ ->
       Or_error.error_string
         [%string "session for participant already exists"])
;;

(* let handle_session_feed (con_state : Connection_state.t):
   (Exchange_event.t Pipe_Reader.t) = let sess = con_state.session in match
   sess with |None -> Error [%string "not logged in"] |Some s ->
   Session.reader s ;; *)

let start ~symbols ~port () =
  let engine = Matching_engine.create symbols in
  let dispatcher = Dispatcher.create () in
  let request_reader, request_writer = Pipe.create () in
  Pipe.set_size_budget request_writer request_queue_size_budget;
  start_matching_loop ~engine ~dispatcher request_reader;
  let implementations =
    Rpc.Implementations.create_exn
      ~implementations:
        [ Rpc.Rpc.implement
            Rpc_protocol.login_rpc
            (fun state participant_str ->
               let reader = handle_login ~dispatcher participant_str state in
               return reader)
        ; Rpc.Rpc.implement
            Rpc_protocol.submit_order_rpc
            (fun state request ->
               handle_submit ~request_writer request state)
        ; Rpc.Rpc.implement' Rpc_protocol.book_query_rpc (fun state symbol ->
            ignore state;
            Matching_engine.book engine symbol
            |> Option.map ~f:Order_book.snapshot)
        ; Rpc.Pipe_rpc.implement
            Rpc_protocol.market_data_rpc
            (fun state symbols ->
               ignore state;
               let reader =
                 Dispatcher.subscribe_market_data dispatcher symbols
               in
               return (Ok reader))
        ; Rpc.Pipe_rpc.implement Rpc_protocol.audit_log_rpc (fun state () ->
            ignore state;
            let reader = Dispatcher.subscribe_audit dispatcher in
            return (Ok reader))
          (*= ; Rpc.Pipe_rpc.implement
            Rpc_protocol.session_feed_rpc
            (fun state participant_str ->
               let reader = handle_login ~dispatcher participant_str state in
               return reader) *)
        ]
      ~on_unknown_rpc:`Close_connection
      ~on_exception:Log_on_background_exn
  in
  let%map tcp_server =
    Rpc.Connection.serve
      ~implementations
      ~initial_connection_state:(fun _addr _conn :
      (Connection_state.t -> { session = None })
        (* don't_wait_for(let%bind _closing_session = Rpc.Connection.close_finished in 
        match conn_state.session with
        |Some sess -> 
        |None -> ())  *)
        conn_state)
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ()
  in
  let actual_port = Tcp.Server.listening_on tcp_server in
  { engine; dispatcher; request_writer; tcp_server; port = actual_port }
;;

let port t = t.port

let close t =
  Pipe.close t.request_writer;
  Tcp.Server.close t.tcp_server
;;

let close_finished t = Tcp.Server.close_finished t.tcp_server
