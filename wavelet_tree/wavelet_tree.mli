module type S =
  sig
    type elt
    (** The type of the wavelet tree elements. *)

    type t
    (** The type of wavelet trees. *)

    (** Takes a list *)
    val create : elt list -> t
      
    val access : t -> int -> elt
      
    val rank : t -> elt -> int -> int option
    (** Returns None if the character isn't in the alphabet *)
      
    val select : t -> elt -> int -> int option
  end
    
module Make (Ord : Set.OrderedType) : S with type elt = Ord.t  
