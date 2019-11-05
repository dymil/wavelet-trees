open Bitv
   
class rank_support bv =
  let log2 a = log (float_of_int a) /. log 2. in
  let log2_n = log2 @@ length bv in
  let x = int_of_float log2_n in
  (* do logs early so that s/b = int(log2 n) or 1 + int(log2 n)) is nice *)
  let s = x * x / 2 and
      b = x / 2 and
      bv_get_chunk arr idx len =
        to_int_us @@ (* should be constant-ish time *)
          let arr' = Printf.printf "bvlen=%d, idx=%d, size=%d\n" (length arr) idx len; (sub arr idx Sys.int_size) in
          fill arr' len (Sys.int_size - len) false; arr' in
  let bv_get_int arr idx size =
    bv_get_chunk arr (idx * size) size and
      bv_get_bit arr idx = if get arr idx then 1 else 0 and
      bv_set_int v arr idx size = blit (of_int_us v) 0 arr (idx * size) size in
  let r_s_el_size = int_of_float @@ ceil @@ log2_n and
      r_b_el_size = int_of_float @@ ceil @@ log2 @@ s and
      r_p_el_size = int_of_float @@ ceil @@ log2 @@ b + 1 in
  object(self)
    val r_s =
      let dim = (s - 1 + length bv) / s in
      let arr = create (dim * r_s_el_size + max 0 (Sys.int_size - r_s_el_size)) false in
      (for j = 1 to dim - 1 do
         let cum_rank =
           let rec rank k = if k = j*s then
                              bv_get_int arr (j-1) r_s_el_size else
                              rank (k-1) + bv_get_bit bv k
           in rank (j*s) in
         bv_set_int cum_rank arr j r_s_el_size
       done; arr)
        val r_b =
          let dim = (b - 1 + length bv) / b in
          let arr = create (dim * r_b_el_size + max 0 (Sys.int_size - r_b_el_size)) false in
          (for j = 1 to dim - 1 do
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
          let arr = create (dim1 * dim2 * r_p_el_size + max 0 (Sys.int_size - r_p_el_size)) false in
          let () = Printf.printf "dim1=%d, dim2=%d length=%d\n" dim1 dim2 @@ length arr in
      (for i = 1 to dim1 - 1 do
         bv_set_int (i mod 2) arr (i * dim2) r_p_el_size;
         for j = 1 to dim2 - 1 do
           bv_set_int ((i lsr j mod 2) + bv_get_int arr (i * dim2 + j - 1) r_p_el_size) arr (i * dim2 + j) r_p_el_size
         done
       done; arr)
        (*i is 1-indexed; rank is inclusive here*)
        method rank1 i = let i = i+1 in let bv' = append bv @@ create (Sys.int_size - 1) true in
                         Printf.printf "rank1: r_s=%d, r_b=%d, r_p=%d\n" (length r_s) (length r_b) (length r_p); let block_idx = (i - 1) / b in
                                                                                                                 bv_get_int r_s ((i - 1) / s) r_s_el_size + bv_get_int r_b block_idx r_b_el_size + bv_get_int r_p (bv_get_chunk bv' (block_idx * b) (i - block_idx * b)) r_p_el_size
        method rank0 i = i - (self#rank1 i)
        method overhead = length r_s + length r_b + length r_p
  end
