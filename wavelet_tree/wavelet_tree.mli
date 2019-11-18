module type WT =
  sig
    type elt
    (** The type of the wavelet tree elements. *)

    type t
    (** The type of wavelet trees. *)

(** Takes a list *)
    val create : elt list -> t
      
    val access : t -> int -> elt
      
    val rank : t -> elt -> int -> int
      
    val select : t -> elt -> int -> int
  end
    
module Make (Ord : Set.OrderedType) : WT with type elt = Ord.t                             
