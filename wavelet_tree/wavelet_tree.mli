type 'a t

(** Takes a list *)
val create : 'a list -> t
  
val access : t -> int -> char
  
val rank : t -> char -> int -> int

val select : t -> char -> int -> int
