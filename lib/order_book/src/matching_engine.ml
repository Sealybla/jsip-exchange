open! Core
open Jsip_types

type t =
  { books : Order_book.t Symbol.Map.t
  ; order_id_gen : Order_id.Generator.t
  ; mutable next_fill_id : int
  ; participant_id_table :
      Order.t Client_order_id.Table.t Participant.Table.t
  }
[@@deriving sexp_of]

let create symbols =
  let books =
    List.map symbols ~f:(fun sym -> sym, Order_book.create sym)
    |> Symbol.Map.of_alist_exn
  in
  { books
  ; order_id_gen = Order_id.Generator.create ()
  ; next_fill_id = 1
  ; participant_id_table = Participant.Table.create ()
  }
;;

let check_add_participant_id t participant client_order_id order : bool =
  match Hashtbl.find t.participant_id_table participant with
  | Some order_ids ->
    (match Hashtbl.add order_ids ~key:client_order_id ~data:order with
     | `Duplicate -> false
     | `Ok -> true)
  | None ->
    let new_order_table = Client_order_id.Table.create () in
    Hashtbl.set new_order_table ~key:client_order_id ~data:order;
    Hashtbl.set t.participant_id_table ~key:participant ~data:new_order_table;
    true
;;

let book t symbol = Map.find t.books symbol

(** Run the matching loop: repeatedly find a compatible resting order and
    fill against it. Returns the list of Fill and Trade_report events
    produced, and the next fill_id to use. *)
let rec match_loop ~book ~order ~fill_id =
  if Size.( <= ) (Order.remaining_size order) Size.zero
  then [], fill_id
  else (
    match Order_book.find_match book order with
    | None -> [], fill_id
    | Some resting ->
      let fill_size =
        Size.min (Order.remaining_size order) (Order.remaining_size resting)
      in
      Order.fill order ~by:fill_size;
      Order.fill resting ~by:fill_size;
      if Order.is_fully_filled resting
      then Order_book.remove book (Order.order_id resting);
      let fill_event =
        Exchange_event.Fill
          { fill_id
          ; symbol = Order.symbol order
          ; price = Order.price resting
          ; size = fill_size
          ; aggressor_order_id = Order.order_id order
          ; aggressor_participant = Order.participant order
          ; aggressor_client_order_id = Order.client_order_id order
          ; aggressor_side = Order.side order
          ; resting_order_id = Order.order_id resting
          ; resting_participant = Order.participant resting
          ; resting_client_order_id = Order.client_order_id order
          }
      in
      let trade_event =
        Exchange_event.Trade_report
          { symbol = Order.symbol order
          ; price = Order.price resting
          ; size = fill_size
          }
      in
      let remaining_events, next_fill_id =
        match_loop ~book ~order ~fill_id:(fill_id + 1)
      in
      fill_event :: trade_event :: remaining_events, next_fill_id)
;;

let submit t (request : Order.Request.t) =
  match Map.find t.books request.symbol with
  | None ->
    [ Exchange_event.Order_reject { request; reason = "unknown symbol" } ]
  | Some book ->
    (* check if valid client_order_id *)
    let order_id = Order_id.Generator.next t.order_id_gen in
    let order = Order.create request ~order_id in
    let client_order_id = Order.Request.client_order_id request in
    let participant = Order.Request.participant request in
    (match check_add_participant_id t participant client_order_id order with
     | false ->
       [ Exchange_event.Order_reject
           { request; reason = "invalid client order id" }
       ]
     | true ->
       let accepted = Exchange_event.Order_accept { order_id; request } in
       (* Snapshot BBO before matching so we can detect changes. *)
       let bbo_before = Order_book.best_bid_offer book in
       (* Match *)
       let fill_events, next_fill_id =
         match_loop ~book ~order ~fill_id:t.next_fill_id
       in
       t.next_fill_id <- next_fill_id;
       (* Post-match: rest on book or cancel unfilled remainder. *)
       let post_events =
         if Size.( > ) (Order.remaining_size order) Size.zero
         then (
           match Order.time_in_force order with
           | Day ->
             Order_book.add book order;
             []
           | Ioc ->
             [ Exchange_event.Order_cancel
                 { order_id
                 ; client_order_id
                 ; participant = Order.participant order
                 ; symbol = Order.symbol order
                 ; remaining_size = Order.remaining_size order
                 ; reason = Ioc_remainder
                 }
             ])
         else []
       in
       (* Emit BBO update if the best bid or ask changed. *)
       let bbo_after = Order_book.best_bid_offer book in
       let bbo_events =
         if Bbo.equal bbo_before bbo_after
         then []
         else
           [ Exchange_event.Best_bid_offer_update
               { symbol = Order.symbol order; bbo = bbo_after }
           ]
       in
       List.concat [ [ accepted ]; fill_events; post_events; bbo_events ])
;;

let cancel_order t ~participant ~client_order_id =
  (* find order by participant and client order id *)
  (* assumes participant must exist *)
  let client_orders = Hashtbl.find t.participant_id_table participant in
  match client_orders with
  | None ->
    (* cancel_reject event *)
    [ Exchange_event.Cancel_reject
        { participant
        ; client_order_id
        ; reason = "client order id does not exist"
        }
    ]
  | Some client_order_ids ->
    let order = Hashtbl.find client_order_ids client_order_id in
    (match order with
     | None ->
       (* cancel_reject event *)
       [ Exchange_event.Cancel_reject
           { participant
           ; client_order_id
           ; reason = "client order id does not exist"
           }
       ]
     | Some ord ->
       [ Exchange_event.Order_cancel
           { order_id = Order.order_id ord
           ; client_order_id
           ; participant
           ; symbol = Order.symbol ord
           ; remaining_size = Order.remaining_size ord
           ; reason = Participant_requested
           }
       ])
;;
