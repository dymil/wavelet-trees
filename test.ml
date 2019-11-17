open OUnit2
   
let bvs = let gen_bv n =
            Random.self_init ();
            Bitv.init n @@ fun _ -> Random.bool () and
              sizes = [(*1; 2; 3;*) 4; 5; 8; 16; 29; 33; 65; 128; 200; 256; 400; 512; 1000; 1024]
                      @ List.init 50 (fun _ -> Random.int 800000) in
          [Bitv.of_int_us @@ Random.bits ();
           Bitv.of_int32_us @@ Random.int32 @@ Int32.max_int;
           Bitv.of_int64_us @@ Random.int64 Int64.max_int]
          @ List.map gen_bv sizes

let bv_get_bit arr idx = if (Bitv.get arr idx) then 1 else 0 and
    n_ops = 2000
          
let test_rank ctx = Random.self_init ();
    Printf.printf "\n==Rank==\n";
    let naive_rank bv i =
      let rec rank_h acc j =
        if j = -1 then acc else rank_h (acc + bv_get_bit bv j) @@ j - 1 in
      rank_h 0 @@ i - 1 in
    let naives = List.map (fun bv ->
                     let rands = Array.init n_ops @@ (fun x ->
                         let rand = 1 + (Random.int @@ Bitv.length bv) in
                         (rand, naive_rank bv rand)) in
                     (bv, rands)) bvs in
    List.iter (fun (bv, rands)  ->
       (* Printf.printf "\n"; for j = 0 to Bitv.length bv - 1 do
          Printf.printf "%d" (if (Bitv.get bv j) then 1 else 0)
                            done; Printf.printf "\n"; *)
        let r = Rank_support.create bv in
        let start = Unix.gettimeofday() in
        for j = 0 to n_ops - 1 do
          let res = Rank_support.rank1 r bv @@ fst rands.(j) in
          OUnit2.assert_equal ~printer:string_of_int (snd rands.(j)) res
        done;
        Printf.printf "Bitv length: %d, Execution time: %f ms, overhead: %d bits\n"
          (Bitv.length bv) (1000. *. (Unix.gettimeofday() -. start)) @@ Rank_support.overhead r
      ) naives
let test_select ctx = Random.self_init ();
  Printf.printf "\n==Select==\n";
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
            let res = if bit then Select_support.select1 s bv rand else
                         Select_support.select0 s bv rand in
            let opt_to_int = function
              |Some n -> n
              |None -> -1 in
            OUnit2.assert_equal ~printer:string_of_int (opt_to_int naive) (opt_to_int res)
        done;
        Printf.printf "Bitv length: %d, Execution time: %f ms, overhead: %d bits\n"
          (Bitv.length bv) (1000. *. (Unix.gettimeofday() -. start)) @@ Select_support.overhead s
      ) naives

let suite =
  "suite">:::
    ["rank">:: test_rank;
     "select">:: test_select]

let () = run_test_tt_main suite