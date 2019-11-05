open Bitv

class rank_support bv =
  let log2 a = log (float_of_int a) /. log 2. in
  let y = log2 @@ length bv in
  let x = int_of_float y in
  (* do logs early so that s/b = int(log2 n) or 1 + int(log2 n)) is nice *)
  let s = x * x / 2 and
      b = x / 2 and
      bv_get_int arr idx size = to_int_us @@ (* should be constant-ish time *)
                                 let bv' = (sub bv (idx * size) 30) in
                                 fill bv' size (30 - size) false; bv' and
      bv_get_bit arr idx = if get bv idx then 1 else 0 and
      bv_set_int v arr idx size = blit (of_int_us v) 0 bv (idx * size) size in
  let r_s_el_size = int_of_float @@ ceil @@ y and
      r_b_el_size = int_of_float @@ log2 @@ b and
      r_p_el_size = int_of_float @@ ceil @@ log2 @@ b+1 in
  object(self)
    val r_s =
      let len = (s - 1 + length bv) / s in
      let arr = create (len * r_s_el_size + max 0 (30 - r_s_el_size)) false in
      (for j = 1 to len do
         let cum_rank =
           let rec rank k = if k = (j*s) then
                              bv_get_int arr (j-1) r_s_el_size else
                              rank (k-1) + bv_get_bit bv k
           in rank (j*s) in
         bv_set_int cum_rank arr j r_s_el_size
       done; arr)
    val r_b =
      let len = (b - 1 + length bv) / b in
      let arr = create (len * r_b_el_size + max 0 (30 - r_b_el_size)) false in
      (for j = 1 to len do
         let local_rank =
           let rec rank k = if k mod (s/b) = 0 then 0 else
                              if k = (j - 1) * b then bv_get_int arr (j-1) r_b_el_size
                              else rank (k-1) + bv_get_bit bv k in rank (j*b) in
         bv_set_int local_rank arr j r_b_el_size
       done; arr)
    (* note r_p is done in little endian order as opposed to the examples *)
    val r_p =
      let dim1 = 1 lsl b and
          dim2 = b in
      let arr = create (dim1 * dim2 * r_p_el_size + max 0 (30 - r_p_el_size)) false in
              (for i = 1 to dim1 do
                 bv_set_int (i mod 2) arr (i * dim2) r_p_el_size;
                 for j = 1 to b do
                   bv_set_int ((i lsr j mod 2) + bv_get_int arr (i * dim2 + j - 1) r_p_el_size) arr (i * dim2 + j) r_p_el_size
                 done
               done; arr)
    (*i is 1-indexed; rank is inclusive here*)
      method rank1 i = let block_idx = (i - 1) / b in
                       bv_get_int r_s ((i - 1) / s) r_s_el_size + bv_get_int r_b block_idx r_b_el_size + bv_get_int r_p (bv_get_int bv (block_idx * b) (i - block_idx * b)) r_p_el_size
    method rank0 i = i - (self#rank1 i)
    method overhead = length r_s + length r_b + length r_p
  end
