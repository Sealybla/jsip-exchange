open! Core
open Jsip_types
open Async_log_kernel.Ppx_log_syntax

type t =
  { symbol : Symbol.t
  ; mutable bids : Order.t list
  ; mutable asks : Order.t list
  }
[@@deriving sexp_of]

let create symbol = { symbol; bids = []; asks = [] }
let symbol t = t.symbol

let side_list t side =
  match (side : Side.t) with Buy -> t.bids | Sell -> t.asks
;;

let set_side_list t side orders =
  match (side : Side.t) with
  | Buy -> t.bids <- orders
  | Sell -> t.asks <- orders
;;

let add t order =
  let side = Order.side order in
  set_side_list t side (order :: side_list t side)
;;

let remove' t order_id =
  let remove_from t side order_id =
    let orders = side_list t side in
    match
      List.partition_tf orders ~f:(fun o ->
        Order_id.equal (Order.order_id o) order_id)
    with
    | [], _ -> None
    | [ found ], rest ->
      set_side_list t side rest;
      Some found
    | matches, _ ->
      [%log.info
        "BUG: More than one order matching order_id found when removing"
          (order_id : Order_id.t)
          (matches : Order.t list)
          (t.symbol : Symbol.t)
          (side : Side.t)];
      None
  in
  match remove_from t Buy order_id with
  | Some _ as result -> result
  | None -> remove_from t Sell order_id
;;

let remove t order_id = ignore (remove' t order_id : Order.t option)

let find t order_id =
  let find_in side =
    List.find (side_list t side) ~f:(fun o ->
      Order_id.equal (Order.order_id o) order_id)
  in
  match find_in Buy with Some _ as result -> result | None -> find_in Sell
;;

(* compares order and other first based on price, then on time return: true:
   -> order more aggressive than other, false -> order less aggressive than
   other *)
let better_pricetime side order other : bool =
  let order_price = Order.price order in
  let other_price = Order.price other in
  match
    Price.is_more_aggressive side ~price:order_price ~than:other_price
  with
  | true -> true
  | false ->
    (match Price.equal order_price other_price with
     (* tie on price -> compare based on time *)
     | true ->
       Order_id.compare (Order.order_id order) (Order.order_id other) > 0
     | false -> false)
;;

(* NOTE: This walks the list front-to-back and returns the *first* tradable
   order, not the best-priced one. Orders are in reverse insertion order
   (newest first), so this matches against whatever was most recently added,
   regardless of price. See test_matching_engine.ml for a test that
   demonstrates why this is wrong. *)
let find_match t incoming =
  let incoming_side = Order.side incoming in
  let opposite_side = Side.flip incoming_side in
  let resting_orders = side_list t opposite_side in
  (* finds newest best order bc starting from beginnign *)
  List.fold resting_orders ~init:None ~f:(fun acc order ->
    match acc with
    | None ->
      (* no marketable orders yet -> check if current order is marketable *)
      if Price.is_marketable
           incoming_side
           ~price:(Order.price incoming)
           ~resting_price:(Order.price order)
      then Some order
      else None
    | Some best ->
      (* check if current order is more aggressive than prev most aggressive
         order *)
      if better_pricetime opposite_side order best
      then Some order
      else Some best)
;;

(* let is_marketable ~price ~resting_price = match (incoming_side : Side.t)
   with | Buy -> Price.( >= ) price resting_price | Sell -> Price.( <= )
   price resting_price in *)
(* List.find resting_orders ~f:(fun resting -> is_marketable
   ~price:(Order.price incoming) ~resting_price:(Order.price resting)) *)

let orders_on_side t side = side_list t side
let is_empty t = List.is_empty t.bids && List.is_empty t.asks
let count t side = List.length (side_list t side)

let best_price t side =
  match side_list t side with
  | [] -> None
  | order_list ->
    let order_with_best_price =
      List.reduce_exn order_list ~f:(fun a b ->
        if Price.is_more_aggressive
             side
             ~price:(Order.price a)
             ~than:(Order.price b)
        then a
        else b)
    in
    let target_price = Order.price order_with_best_price in
    Some
      (Order.price
         (List.find_exn order_list ~f:(fun order ->
            Price.equal (Order.price order) target_price)))
;;

(* | first :: rest -> let is_better = match (side : Side.t) with Buy ->
   Price.( > ) | Sell -> Price.( < ) in Some (List.fold rest
   ~init:(Order.price first) ~f:(fun best order -> let price = Order.price
   order in if is_better price best then price else best)) (List.reduce rest) *)

let best_level t side : Level.t option =
  match best_price t side with
  | None -> None
  | Some price ->
    let total_size =
      List.fold (side_list t side) ~init:Size.zero ~f:(fun acc order ->
        if Price.equal (Order.price order) price
        then Size.( + ) acc (Order.remaining_size order)
        else acc)
    in
    Some { price; size = total_size }
;;

let best_bid_offer t : Bbo.t =
  { bid = best_level t Buy; ask = best_level t Sell }
;;

let snapshot_side t (side : Side.t) =
  let compare order other =
    if better_pricetime side order other then -1 else 1
  in
  orders_on_side t side |> List.sort ~compare |> List.map ~f:Level.of_order
;;

(* will stable_sort work without accounting for time *)

let snapshot t =
  { Book.symbol = symbol t
  ; bids = snapshot_side t Buy
  ; asks = snapshot_side t Sell
  ; bbo = best_bid_offer t
  }
;;

module For_testing = struct
  let remove = remove'
end
