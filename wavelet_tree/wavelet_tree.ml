(** sorted character alphabet, root node*)
type 'a t = 'a array * btree

(** Select_support.t isn't included, as it's the same as Rank_support.t here *)
type btree = Leaf of int | Node of Bitv.t * Rank_support.t * btree * btree

(** This is the naive partition-based construction algorithm. *)
let create s =
  let arr =
    let module ASet = Set.Make('a) in
    let set = ASet.of_list s in
    Array.of_seq (ASet.to_seq set) in
  let rec bsearch low high c =
    if low > high then failwith "Array didn't contain a charcter in the list"
    else
      let mid = (low + high) / 2 in
      if arr.(mid) = c then mid
      else if arr.(mid) > c || then bsearch low (mid - 1) c
      else bsearch (mid + 1) high c in
  let list = List.rev @@ List.rev_map (bsearch 0 Array.length arr) s in
  let rec make_tree list' bv start fin =
    if start + 1 = fin then Leaf start else
      let len_left = ref 0 and idx = ref -1 in
      let mid = (start + fin) / 2 in
      let left, right = List.partition (fun el ->
                            idx := !idx + 1;
                            if el <= mid then
                              len_left := len_left + 1; true
                            else
                              Bitv.set bv !idx true; false) list' in
      let len_right = (Bitv.length bv) - len_left in
      Node(bv, Rank_support.create bv,
           make_tree left (Bitv.create len_left false) start (mid + 1),
           make_tree right (Bitv.create len_right false) (mid + 1) fin) in
  let len = List.length list in
  (arr, make_tree list (Bitv.create len false) 0 len)
    
let access wt i = ()

let rank wt c i = ()

let select wt c i = ()
