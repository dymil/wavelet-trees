open OUnit2
   
let bvs = let gen_bv n =
            Random.init @@ int_of_float @@ Unix.time ();
            Bitv.init n @@ fun _ -> Random.bool () and
              sizes = [1; 2; 3; 4; 5; 8; 16; 29; 33; 65; 128; 200; 256; 400; 512; 1000; 1024]
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
                     let rands = Array.init n_ops @@ (fun _ ->
                         let rand = 1 + (Random.int @@ Bitv.length bv) in
                         (rand, naive_rank bv rand)) in
                     (bv, rands)) bvs in
    List.iter (fun (bv, rands)  ->
       (* Printf.printf "\n"; for j = 0 to Bitv.length bv - 1 do
          Printf.printf "%d" (if (Bitv.get bv j) then 1 else 0)
                            done; Printf.printf "\n"; *)
        let r = Rank_support.create bv in
        let start = Unix.gettimeofday() in
        Array.iter begin fun rand ->
          let res = Rank_support.rank1 r bv @@ fst rand in
          OUnit2.assert_equal ~printer:string_of_int (snd rand) res
          end rands;
        Printf.printf "Bitv length: %d, Execution time: %f ms, overhead: %d bits\n"
          (Bitv.length bv) (1000. *. (Unix.gettimeofday() -. start)) @@ Rank_support.overhead r
      ) naives
let test_select ctx =
  Random.init @@ int_of_float @@ Unix.time ();
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
        Array.iter
          begin function
            | (rand, bit, naive) ->
               let res = if bit then Select_support.select1 s bv rand else
                           Select_support.select0 s bv rand in
               let opt_to_int = function
                 | Some n -> n
                 | None -> -1 in
               OUnit2.assert_equal ~printer:string_of_int (opt_to_int naive) (opt_to_int res)
          end rands;
        Printf.printf "Bitv length: %d, Execution time: %f ms, overhead: %d bits\n"
          (Bitv.length bv) (1000. *. (Unix.gettimeofday() -. start)) @@ Select_support.overhead s
      ) naives
        
let test_wt ctx =
  let module CharWT = Wavelet_tree.Make(Char) in
  let wt = CharWT.build ("alabar a la alabarda" |> String.to_seq |> List.of_seq) in
  OUnit2.assert_equal ~printer:string_of_int 5 @@ Option.get @@ CharWT.rank wt 'a' 11;
  OUnit2.assert_equal ~printer:string_of_int 14 @@ Option.get @@ CharWT.select wt 'l' 3;
  OUnit2.assert_equal ~printer:(Printf.sprintf "%c") 'r' @@ CharWT.access wt 6

let char_lists =
  Random.self_init ();
  let num = 60 in
  let sigmas= [1; 2; 3; 4; 5] @ List.init num (fun _ -> 1 + Random.int 255)
    and
      strlens = [1; 2; 3; 4; 5] @ List.init num (fun _ -> 1 + Random.int 40000) in
  List.fold_left2 (fun acc sigma n -> (sigma, List.init n (fun _ -> Char.chr @@ Random.int sigma))::acc) [] sigmas strlens
  
let test_wt_rand_rank ctx =
  Random.init @@ int_of_float @@ Unix.time ();
  Printf.printf "\n==Rank==\n";
  let naive_rank l c i =
    let rec rank_h l' j acc = match l' with
      | h::_ when j = 1 -> acc + (if h = c then 1 else 0)
      | h::t when j > 1 -> rank_h t (j - 1) (if h = c then acc + 1 else acc)
      | _ -> failwith "can't happen" in
    rank_h l i 0 in
  let naives = List.map (fun (sigma, l) ->
                   let rands = List.init n_ops @@ (fun _ ->
                       let c = Char.chr @@ Random.int sigma and
                           i = 1 + (Random.int @@ List.length l) in
                       (c, i, naive_rank l c i)) in
                   (sigma, l, rands)) char_lists in
  let module CharWT = Wavelet_tree.Make(Char) in
  List.iter (fun (sigma, l, rands) ->
      let wt = CharWT.build l in
      let start = Unix.gettimeofday() in
      List.iter (
          function (c, i, naive) ->
            let opt_to_int = function
              | Some n -> n
              | None -> 0 in
            let res = CharWT.rank wt c i in
            OUnit2.assert_equal ~printer:string_of_int naive @@ opt_to_int res) rands;
      Printf.printf "List length=%d, sigma=%d, time=%f ms\n" (List.length l) sigma @@ 1000. *. (Unix.gettimeofday() -. start)) naives

let test_wt_rand_select ctx =
  Random.init @@ int_of_float @@ Unix.time ();
  Printf.printf "\n==Select==\n";
  let naive_select l c i =
    let rec help l' idx acc = match l' with
      | [] -> None
      | h::t ->
         let acc' = acc + (if c = h then 1 else 0) in
         if acc' = i then Some idx else
           help t (idx + 1) acc' in
    help l 1 0 in
    let naives = List.map (fun (sigma, l) ->
                   let rands = List.init n_ops @@ (fun _ ->
                       let c = Char.chr @@ Random.int sigma and
                           i = 1 + (Random.int @@ List.length l) in
                       (c, i, naive_select l c i)) in
                   (sigma, l, rands)) char_lists in
    let module CharWT = Wavelet_tree.Make(Char) in
    List.iter (fun (sigma, l, rands) ->
        let wt = CharWT.build l in
        let start = Unix.gettimeofday() in
        List.iter (
            function (c, i, naive) ->
              let res = CharWT.select wt c i in
              let opt_to_int = function
                | Some n -> n
                | None -> -1 in
              OUnit2.assert_equal ~printer:string_of_int (opt_to_int naive) (opt_to_int res)) rands;
        Printf.printf "List length=%d, sigma=%d, time=%f ms\n" (List.length l) sigma @@ 1000. *. (Unix.gettimeofday() -. start)) naives

let suite =
  "suite">:::
    ["rank">:: test_rank;
     "select">:: test_select;
     "wt">:: test_wt;
     "wt_rank">:: test_wt_rand_rank;
     "wt_select">:: test_wt_rand_select]

let () = run_test_tt_main suite
