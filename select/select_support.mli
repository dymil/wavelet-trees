type t

val create : Rank_support.t -> t

val select1 : t -> int -> int option

val select0 : t -> int -> int option

val overhead : t -> int
