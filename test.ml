let () = Random.self_init () in
    Printf.printf "==Rank==\n";
    let gen_bv n = Bitv.init n @@ fun _ -> Random.bool () and
        bv_get_bit arr idx = if (Bitv.get arr idx) then 1 else 0 and
        n_ops = 10 in
    let sizes = [(*1; 2; 3;*) 4; 5; 29; 8; 16; 128; 256; 31; 33; 65; 200; 256; 512; 400; 1024; 2048] in
    let bvs = [Bitv.of_int_us @@ Random.bits ();
               Bitv.of_int32_us @@ Random.int32 @@ Int32.max_int;
               Bitv.of_nativeint_us @@ Random.nativeint @@ Nativeint.max_int;
               Bitv.of_int64_us @@ Random.int64 Int64.max_int]
              @ List.map gen_bv sizes in
    let naive_rank bv i =
      let rec rank_h j =
        if j = -1 then 0 else (bv_get_bit bv j) + (rank_h @@ j - 1) in
      rank_h @@ i-1 in
    let naives = List.map (fun bv ->
                     let rands = Array.init n_ops @@ (fun x ->
                         let rand = 1 + (Random.int @@ Bitv.length bv) in
                         (rand, naive_rank bv rand)) in
                     (bv, rands)) bvs in
    List.iter (fun (bv, rands)  ->
        Printf.printf "\n"; for j = 0 to Bitv.length bv - 1 do
          Printf.printf "%d" (if (Bitv.get bv j) then 1 else 0)
                            done; Printf.printf "\n";
        let r = Rank_support.create bv in
        let start = Unix.gettimeofday() in
        for j = 0 to n_ops - 1 do
          let res = Rank_support.rank1 r @@ fst rands.(j) in
          OUnit2.assert_equal ~printer:string_of_int (snd rands.(j)) res
        done; Printf.printf "Bitv length: %d, Execution time: %f sec, overhead: %d bits\n" (Bitv.length bv) (Unix.gettimeofday() -. start) @@ Rank_support.overhead r
      ) naives;
    Printf.printf "==Select==\n";
    let naive_select bv i b =
      let rec help idx acc =
        if idx > Bitv.length bv then None else
          let bit = Bitv.get bv (idx - 1) in
          let acc' = acc + (if bit = b then 1 else 0) in
          if acc' = i then Some idx else
            help (idx + 1) acc' in
      help 1 0 in
    let naives = List.map (fun bv ->
                     let rands =
                       Array.init n_ops @@ (fun _ ->
                         let rand = 1 + (Random.int @@ Bitv.length bv)
                         and bit = Random.bool () in
                         (rand, bit, naive_select bv rand bit)) in
                     (bv, rands)) bvs in
    List.iter (fun (bv, rands)  ->
        (*Printf.printf "\n";
        for j = 0 to Bitv.length bv - 1 do
          Printf.printf "%d" (if (Bitv.get bv j) then 1 else 0)
        done; Printf.printf "\n";*)
        let s = Select_support.create @@ Rank_support.create bv in
        let start = Unix.gettimeofday() in
        for j = 0 to n_ops - 1 do
          match rands.(j) with
          |(rand, bit, naive) ->
            let res = (if bit then Select_support.select1 s rand else Select_support.select0 s rand) in
            let opt_to_int = function
              |Some n -> n
              |None -> -1 in
            OUnit2.assert_equal ~printer:string_of_int (opt_to_int naive) (opt_to_int res)
        done; Printf.printf "Bitv length: %d, Execution time: %f sec, overhead: %d bits\n" (Bitv.length bv) (Unix.gettimeofday() -. start) @@ Select_support.overhead s
      ) naives
