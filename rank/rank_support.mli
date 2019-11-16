type t

val create Bitv.t -> t

val rank1 : t -> int -> int

val rank0 : t -> int -> int

val overhead: t -> int
