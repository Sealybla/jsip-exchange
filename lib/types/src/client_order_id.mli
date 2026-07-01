open! Core

type t [@@deriving sexp, bin_io, compare, equal, hash, string]

val of_int : int -> t

include Comparable.S with type t := t
include Hashable.S with type t := t
