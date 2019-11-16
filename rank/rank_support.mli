type t = {
    r_s : Bitv.t;
    r_b : Bitv.t;
    r_p : Bitv.t ;
    bv' : Bitv.t;
  }

val create : Bitv.t -> t

val rank1 : t -> int -> int

val rank0 : t -> int -> int

val overhead: t -> int
