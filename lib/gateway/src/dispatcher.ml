open! Core
open! Async
open Jsip_types

type t =
  { market_data_subscribers_by_symbol :
      Exchange_event.t Pipe.Writer.t Bag.t Symbol.Table.t
  ; audit_subscribers : Exchange_event.t Pipe.Writer.t Bag.t
  ; participant_sessions : Session.t Participant.Table.t
  }

let create () =
  { market_data_subscribers_by_symbol = Symbol.Table.create ()
  ; audit_subscribers = Bag.create ()
  ; participant_sessions = Participant.Table.create ()
  }
;;

let subscribe_market_data t symbols =
  let reader, writer = Pipe.create () in
  (* Register the same writer in every requested symbol's bag. A per-symbol
     publish iterates a single bag, so a subscriber listed in multiple bags
     event's symbol. *)
  let elts =
    List.map symbols ~f:(fun symbol ->
      let subscribers =
        Hashtbl.find_or_add
          t.market_data_subscribers_by_symbol
          ~default:Bag.create
          symbol
      in
      symbol, Bag.add subscribers writer)
  in
  don't_wait_for
    (let%map () = Pipe.closed writer in
     List.iter elts ~f:(fun (symbol, elt) ->
       match Hashtbl.find t.market_data_subscribers_by_symbol symbol with
       | None -> ()
       | Some subscribers -> Bag.remove subscribers elt));
  reader
;;

let subscribe_audit t =
  let reader, writer = Pipe.create () in
  let elt = Bag.add t.audit_subscribers writer in
  don't_wait_for
    (let%map () = Pipe.closed writer in
     Bag.remove t.audit_subscribers elt);
  reader
;;

let push_market_data t event symbol =
  match Hashtbl.find t.market_data_subscribers_by_symbol symbol with
  | None -> ()
  | Some subscribers ->
    Bag.iter subscribers ~f:(fun writer ->
      Pipe.write_without_pushback_if_open writer event)
;;

let push_audit t event =
  Bag.iter t.audit_subscribers ~f:(fun writer ->
    Pipe.write_without_pushback_if_open writer event)
;;

let push_to_session t participant event =
  (* TODO: Once sessions have been implemented this function should write the
     event to the appropriate session's pipe. For now we have the server
     binary print these events to stdout while tests can silence them. *)

  (* Checks participant exists in participant sessions first *)
  let participant_sess = Hashtbl.find t.participant_sessions participant in
  match participant_sess with
  | Some sess -> Session.push sess event
  | None -> ()
;;

let dispatch_event t (event : Exchange_event.t) =
  push_audit t event;
  match event with
  | Best_bid_offer_update { symbol; bbo = _ } ->
    push_market_data t event symbol
  | Trade_report { symbol; price = _; size = _ } ->
    push_market_data t event symbol
  | Order_accept { order_id = _; request }
  | Order_reject { request; reason = _ } ->
    push_to_session t request.participant event
  | Order_cancel
      { order_id = _
      ; client_order_id = _
      ; participant
      ; symbol = _
      ; remaining_size = _
      ; reason = _
      } ->
    push_to_session t participant event
  | Fill
      { fill_id = _
      ; symbol = _
      ; price = _
      ; size = _
      ; aggressor_order_id = _
      ; aggressor_participant
      ; aggressor_client_order_id = _
      ; aggressor_side = _
      ; resting_order_id = _
      ; resting_participant
      ; resting_client_order_id = _
      } ->
    push_to_session t aggressor_participant event;
    push_to_session t resting_participant event
;;

let dispatch t events = List.iter events ~f:(dispatch_event t)

module For_testing = struct
  let audit_subscriber_count t = Bag.length t.audit_subscribers
end

let clean_up_session_helper t (session : Session.t) : unit =
  (* check if session exists using participant bc bijection btw participants
     and sessions *)
  let exist_session_or_none =
    Hashtbl.find_and_remove
      t.participant_sessions
      (Session.participant session)
  in
  match exist_session_or_none with
  | Some sess -> Session.close sess
  | None -> ()
;;

let clean_up_session t (session : Session.t) : unit Deferred.t =
  Deferred.return (clean_up_session_helper t session)
;;

let set_up_session_helper t (participant : Participant.t) : unit =
  (* replace so participant is automatically removed then added (or just
     added if DNE in table) *)
  let sess_if_exists = Hashtbl.find t.participant_sessions participant in
  (match sess_if_exists with
   | Some sess -> clean_up_session_helper t sess
   | None -> ());
  Hashtbl.add_exn
    t.participant_sessions
    ~key:participant
    ~data:(Session.create participant)
;;

let set_up_session_err t (participant : Participant.t) : Session.t Or_error.t
  =
  let sess_if_exists = Hashtbl.find t.participant_sessions participant in
  match sess_if_exists with
  | Some _ ->
    Or_error.error_string
      [%string "conflict: participant already has session"]
  | None ->
    let new_sess = Session.create participant in
    Hashtbl.add_exn t.participant_sessions ~key:participant ~data:new_sess;
    Ok new_sess
;;

(* write clean up session helper without deferred, clean up session *)
let set_up_session t (participant : Participant.t) : unit Deferred.t =
  Deferred.return (set_up_session_helper t participant)
;;
