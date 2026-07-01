open! Core

type t = int [@@deriving sexp, bin_io, compare, equal, hash, string]

let of_int = Fn.id

include functor Comparable.Make
include functor Hashable.Make
