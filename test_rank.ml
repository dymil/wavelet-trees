let () = Random.self_init () in
    let bv_get_bit arr idx = if (Bitv.get arr idx) then 1 else 0 and
        n_ops = 1 in
    let sizes = [(*1; *)2; 4; 8; 16; 128; 256; 31; 33; 62; 63; 65] in
    let sizes = [4] in
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
        if j = -1 then 0 else
          let r = (bv_get_bit bv j) + (rank_h @@ j - 1) in
          (Printf.printf "get=%B, j=%d, r=%d\t" (Bitv.get bv j) j r;
          r) in
      (Printf.printf "\n\n"; rank_h @@ i+1) in
    let naives = List.map (fun bv -> let rands = Array.init n_ops @@ (fun x -> let rand = 2 (*Random.int @@ Bitv.length bv*) in (rand, naive_rank bv rand)) in
                                     (bv, rands)) bvs in
    List.iter (fun (bv, rands)  ->
        Printf.printf "\n"; for j = 0 to Bitv.length bv - 1 do
          Printf.printf "%d" (if (Bitv.get bv j) then 1 else 0)
        done; Printf.printf "\n";
        let r = new Rank_support.rank_support bv in
        let start = Unix.gettimeofday() in
        for j = 0 to n_ops - 1 do
          Printf.printf "len=%d, i=%d\n" (Bitv.length bv) @@ fst rands.(j);
          let res = r#rank1 @@ fst rands.(j) in
          OUnit2.assert_equal ~printer:string_of_int res @@ snd rands.(j)
        done; Printf.printf "Execution time %f sec, overhead %d bits" (Unix.gettimeofday() -. start) r#overhead
      ) naives
