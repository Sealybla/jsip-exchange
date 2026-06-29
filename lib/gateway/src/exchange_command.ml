open! Core
open Jsip_types

module Verb = struct
  type t =
    | Buy
    | Sell
    | Book
    | Subscribe
  [@@deriving string ~case_insensitive]
end

type t =
  | Submit of Order.Request.t
  | Book of Symbol.t
  | Subscribe of Symbol.t
[@@deriving sexp]
(* ask what is sexp and deriving stuff *)

let get_symb_book_sub rest : Symbol.t Or_error.t =
  match rest with
  | [] -> Or_error.error_string "empty command"
  | symbol_str :: _ ->
    (try Ok (Symbol.of_string symbol_str) with
     | exn ->
       let exn_str = Exn.to_string exn in
       Or_error.error_string
         [%string "invalid symbol: %{symbol_str}\nexception: %{exn_str}"])
;;

let parse
  ?(default_participant : Participant.t = Participant.of_string "anonymous")
  line
  : t Or_error.t
  =
  let line = String.strip line in
  if String.is_empty line
  then Or_error.error_string "empty command"
  else (
    let parts =
      String.split line ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
    in
    match parts with
    | [] -> Or_error.error_string "empty command"
    | verb_str :: rest ->
      let open Or_error.Let_syntax in
      (* parse first string as verb, if fail -> err *)
      let%bind (side_first : Verb.t) =
        Or_error.try_with (fun () -> Verb.of_string verb_str)
      in
      (match side_first with
       | Buy | Sell ->
         (match rest with
          | symbol_str :: size_str :: price_str :: rest ->
            let%bind size =
              match Int.of_string_opt size_str with
              | Some n when n > 0 -> Ok n
              | Some _ ->
                Or_error.error_string
                  "size must be positive" (* sexp error *)
              | None ->
                Or_error.error_string [%string "invalid size: %{size_str}"]
            in
            let%bind price =
              try Ok (Price.of_string price_str) with
              | exn ->
                let exn_str = Exn.to_string exn in
                Or_error.error_string
                  [%string
                    "invalid price: %{price_str}\nexception: %{exn_str}"]
            in
            let%bind symbol =
              try Ok (Symbol.of_string symbol_str) with
              | exn ->
                let exn_str = Exn.to_string exn in
                Or_error.error_string
                  [%string
                    "invalid symbol: %{symbol_str}\nexception: %{exn_str}"]
            in
            let%bind (time_in_force : Time_in_force.t), rest =
              match rest with
              | tif_str :: rest' ->
                (* add match to check if time in force succeeds, --> either
                   IOC/DAY or everything else *)
                let is_book_sub =
                  Or_error.try_with (fun () ->
                    Time_in_force.of_string tif_str)
                in
                (match is_book_sub with
                 | Error _ ->
                   (match String.uppercase tif_str with
                    | "AS" -> Ok (Time_in_force.Day, rest')
                    | _ ->
                      Or_error.error_string
                        [%string
                          "unknown time-in-force: %{tif_str} (expected DAY \
                           or IOC)"])
                 | Ok _ -> Ok (ok_exn is_book_sub, rest))
                (* how to create separate cases for AS vs IOC/DAY *)
              | [] -> Ok (Day, [])
            in
            let%bind participant =
              match rest with
              (* Exchange only takes as and AS *)
              (* extra stuff after name -> ignore or error (exchange choice) *)
              | "as" :: name :: _ | "AS" :: name :: _ ->
                Ok (Participant.of_string name)
              | [] -> Ok default_participant
              | _ ->
                let trailing = String.concat ~sep:" " rest in
                Or_error.error_string
                  [%string "unexpected trailing arguments: %{trailing}"]
            in
            Ok
              (Submit
                 { symbol
                 ; participant
                 ; client_order_id = 0 (* NOT TRUE WILL CHANGE LATER *)
                 ; side =
                     (match side_first with
                      | Buy -> Side.Buy
                      | _ -> Side.Sell)
                     (* other side will only be sell *)
                 ; price
                 ; size = Size.of_int size
                 ; time_in_force
                 })
          | _ ->
            Or_error.error_string
              "expected: BUY|SELL <symbol> <size> <price> [DAY|IOC] [as \
               <name>]")
       | Book ->
         let symb = get_symb_book_sub rest in
         Ok (Book (ok_exn symb))
       | Subscribe ->
         let symb = get_symb_book_sub rest in
         Ok (Subscribe (ok_exn symb))))
;;

(* modularize later *)
