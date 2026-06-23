open! Core
open Jsip_types

module Verb : sig
  type t =
    | Buy
    | Sell
    | Book
    | Subscribe
  [@@deriving string]
end

type t =
  | Submit of Order.Request.t
  | Book of Symbol.t
  | Subscribe of Symbol.t
[@@deriving sexp]
(* ask what is sexp and deriving stuff *)

(* Splits the line on spaces, take the first word Parse it as Verb.t If it
   fails, return Error Match on verb to parse remaining args: Buy | Sell or
   Book | Subscribe *)
val parse : ?default_participant:Participant.t -> string -> t Or_error.t
