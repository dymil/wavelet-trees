let () = Random.self_init () in
    let bv_get_bit arr idx = if (Bitv.get arr idx) then 1 else 0 and
        n_ops = 1000 in
    let sizes = [(*1; 2; *)4; 8; 16; 128; 256; 31; 33; 65] in
    let bvs = [Bitv.of_int_us @@ Random.bits ();
               Bitv.of_int32_us @@ Random.int32 @@ Int32.max_int;
               Bitv.of_nativeint_us @@ Random.nativeint @@ Nativeint.max_int;
               Bitv.of_int64_us @@ Random.int64 Int64.max_int]
              @ List.map (fun n ->
                    Bitv.of_list_with_length (
                        let rec f k l =
                          if k = 0 then l else
                            Random.int n :: (f (k-1) l) in
                        f n []) n) sizes in
    let naive_rank bv i =
      let rec rank_h j =
        if j = -1 then 0 else (bv_get_bit bv j) + (rank_h @@ j - 1) in
      rank_h @@ i-1 in
    let naives = List.map (fun bv -> let rands = Array.init n_ops @@ (fun x -> let rand = 1 + (Random.int @@ Bitv.length bv - 1) in (rand, naive_rank bv rand)) in
                                     (bv, rands)) bvs in
    List.iter (fun (bv, rands)  ->
        (*Printf.printf "\n"; for j = 0 to Bitv.length bv - 1 do
          Printf.printf "%d" (if (Bitv.get bv j) then 1 else 0)
        done; Printf.printf "\n";*)
        let r = new Rank_support.rank_support bv in
        let start = Unix.gettimeofday() in
        for j = 0 to n_ops - 1 do
          (*Printf.printf "len=%d, i=%d\n" (Bitv.length bv) @@ fst rands.(j);*)
          let res = r#rank1 @@ fst rands.(j) in
          OUnit2.assert_equal ~printer:string_of_int (snd rands.(j)) res
        done; Printf.printf "Bitv length: %d, Execution time: %f sec, overhead: %d bits\n" (Bitv.length bv) (Unix.gettimeofday() -. start) r#overhead
      ) naives
