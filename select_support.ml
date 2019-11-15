
class select_support bv =
object(self)
  inherit Rank_support.rank_support bv
  method private select i b =
    let rec bsearch low high =
      if low > high then None else
        let mid = (low + high) / 2 in
        let rank = if b then self#rank1 mid else self#rank0 mid in
        if rank = i && b = Bitv.get bv @@ mid - 1 then Some mid
        else if rank > i || (rank = i && (b <> Bitv.get bv @@ mid - 1)) then bsearch low @@ mid - 1
        else bsearch (mid + 1) high in
    bsearch 1 @@ Bitv.length bv
  method select1 i = self#select i true
  method select0 i = self#select i false
end
