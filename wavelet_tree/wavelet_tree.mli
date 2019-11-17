type t

val create : string -> t
  
val access : t -> int -> char
  
val rank : t -> char -> int -> int

val select : t -> char -> int -> int
