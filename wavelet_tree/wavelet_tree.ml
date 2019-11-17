(** sorted character alphabet, root node*)
type t = string * btree

type btree = Leaf of char | Node of Bitv.t * Rank_support.t * Select_support.t * btree * btree

let create s = ()

let access wt i = ()

let rank wt c i = ()

let select wt c i = ()
