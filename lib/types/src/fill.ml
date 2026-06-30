open! Core

type t =
  { fill_id : int
  ; symbol : Symbol.t
  ; price : Price.t
  ; size : Size.t
  ; aggressor_order_id : Order_id.t
  ; aggressor_participant : Participant.t
  ; aggressor_client_order_id : Client_order_id.t
  ; aggressor_side : Side.t
  ; resting_order_id : Order_id.t
  ; resting_participant : Participant.t
  ; resting_client_order_id : Client_order_id.t
  }
[@@deriving sexp, bin_io]

let to_string
  ({ fill_id
   ; symbol
   ; price
   ; size
   ; aggressor_order_id
   ; aggressor_participant (* do not need order ID to be revealed *)
   ; aggressor_client_order_id = _
   ; aggressor_side
   ; resting_order_id
   ; resting_participant
   ; resting_client_order_id = _
   } :
    t)
  =
  sprintf
    "fill_id=%d %s %s x%d aggressor=%s(%s) %s resting=%s(%s)"
    fill_id
    (Symbol.to_string symbol)
    (Price.to_string_dollar price)
    (Size.to_int size)
    (Order_id.to_string aggressor_order_id)
    (Participant.to_string aggressor_participant)
    (Side.to_string aggressor_side)
    (Order_id.to_string resting_order_id)
    (Participant.to_string resting_participant)
;;

let notional_cents t = Price.to_int_cents t.price * Size.to_int t.size

let get_str_given_side t side : string =
  let side_str = if Side.equal side Side.Buy then "bought" else "sold" in
  [%string
    "You %{side_str} %{Size.to_string t.size} %{Symbol.to_string t.symbol} \
     at $ %{Price.to_string t.price}."]
;;

let to_participant_view t participant : string option =
  (* what if participant trades with themself -> use tuples? *)
  match Participant.equal participant t.aggressor_participant with
  | true ->
    let side = t.aggressor_side in
    Some (get_str_given_side t side)
  | false ->
    (match Participant.equal participant t.resting_participant with
     | true ->
       let side = Side.flip t.aggressor_side in
       Some (get_str_given_side t side)
     | false -> None)
;;
