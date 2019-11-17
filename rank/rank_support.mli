type t

val create : Bitv.t -> t

(** Results are undefined if given a different bitvector *)
val rank1 : t -> Bitv.t -> int -> int

val rank0 : t -> Bitv.t -> int -> int

val overhead: t -> int
