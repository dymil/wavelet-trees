module Make(Ord: Set.OrderedType) =
  struct
    type elt = Ord.t
             
    (** sorted character alphabet, root node*)
    type 'a t = 'a array * btree
              
    (** Waste a little extra space including select support, but it just holds pointers to the same arrays as rank support *)
    type btree = Leaf of int | Node of Bitv.t * Rank_support.t * Select_support.t *  btree * btree
                                     
    let rec bsearch arr low high c =
      if low >= high then failwith "Array didn't contain a charcter in the list"
      else
        let mid = (low + high) / 2 in
        if arr.(mid) = c then mid
        else if arr.(mid) > c then bsearch low (mid - 1) c
        else bsearch (mid + 1) high c
        
    (** This is the naive partition-based construction algorithm. *)
    let create s =
      let arr =
        let module ASet = Set.Make(Ord) in
        let set = ASet.of_list s in
        Array.of_seq (ASet.to_seq set) in
      let list = List.rev @@ List.rev_map (bsearch arr 0 @@ Array.length arr) s in
      let rec make_tree list' bv start fin =
        if start + 1 = fin then Leaf start else
          let len_left = ref 0 and idx = ref -1 in
          let mid = (start + fin) / 2 in
          let left, right = List.partition (fun el ->
                                idx := !idx + 1;
                                if el <= mid then begin
                                    len_left := len_left + 1; true
                                end else begin
                                    Bitv.set bv !idx true; false end) list' in
          let len_right = (Bitv.length bv) - len_left and
              r = Rank_support.create bv in
          Node(bv, r, Select_support.create r,
               make_tree left (Bitv.create len_left false) start (mid + 1),
               make_tree right (Bitv.create len_right false) (mid + 1) fin) in
      let len = List.length list in
      (arr, make_tree list (Bitv.create len false) 0 len)
      
    let access wt i =
      let rec access_h tree i = match tree with
        | Leaf n -> (fst wt).(n)
        | Node (bv, r, left, right) ->
           if Bitv.get bv i then access_h left @@ Rank_support.rank0 r bv i
           else access_h right @@ Rank_support.rank1 r bv i in
      access_h (snd wt) i

    let rank wt c i =
      let c' = bsearch (fst wt) 0 (Array.length @@ fst wt) c in
      (* start, fin are indices in the alphabet *)
      let rec rank_h tree start fin i = match tree with
        | Leaf _ -> None
        | Node (bv, r, _, left, right) ->
           Option.get @@
             let mid = (start + fin) / 2 in
             let which_child = c' > mid in
             let rank = if which_child then Rank_support.rank1 r bv i
                        else Rank_support.rank0 r bv i in
             let res = (if which_child then rank_h right start (mid + 1) else
                          rank_h left (mid + 1) fin) rank in
             if Option.is_none res then Some rank else res in
      rank_h (snd wt) 0 @@ Array.length @@ fst wt

    let select wt c i =
      let c' = bsearch (fst wt) 0 (Array.length @@ fst wt) c in
      let rec select_h tree start fin acc = match tree with
        | Leaf n ->
           List.fold_left (fun acc' (bv, s, b) ->
               Option.bind acc' @@
                 (if b then Select_support.select1 else Select_support.select0) s bv)
             (Some i) acc
        | Node (bv, _, s, left, right) ->
           let mid = (start + fin) / 2 in
           if c' <= mid then
             select_h left start (mid + 1) (bv, s, false)::acc
           else
             select_h right (mid + 1) fin (bv, s, true)::acc
end
