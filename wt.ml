(** Command-line program which supports serializing and deserializing
    wavelet trees over a character alphabet.
    Note that Bitv is implemented using ints, so serialized files are not portable across 32-bit and 64-bit architectures. *)
open Printf

let usage_msg = "Usage: wt cmd arg1 arg2\nCommands available:\nbuild <input string> <output file>\naccess <saved wt> <access indices>\nwhere <access indices> is a file containing the indices, each 0-indexed on a separate line\nrank <saved wt> <rank queries>\nwhere <rank queries> is a file containing the queries of format <c>\t<i> where i is 0-indexed\nselect <saved wt> <select queries>\nwhere select queries is of the form of rank queries above"

module CharWT = Wavelet_tree.Make(Char)

let build s ofile =
  let l = String.to_seq s |> List.of_seq in
  let wt = CharWT.build l in
  printf "Alphabet size: %d\nString length: %d\n" (CharWT.alphabet_size wt) (String.length s);
  let oc = open_out_bin ofile in
  try
    Marshal.to_channel oc wt [];
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

let iter_lines f wt_file ifile =
  let ic = open_in_bin wt_file in
  try
    let wt : CharWT.t = Marshal.from_channel ic in
    close_in ic;
    let ic = open_in ifile in
    try
      while true do
        let line = input_line ic in
        f wt line
      done
    with
    | End_of_file -> close_in_noerr ic
    | e -> close_in_noerr ic; raise e
  with
  | e -> close_in_noerr ic; raise e

let access wt_file indices_file =
  iter_lines
    (fun wt line -> printf "%c\n" @@ CharWT.access wt @@ 1 + int_of_string line)
    wt_file
    indices_file

let rank wt_file query_file =
  iter_lines (fun wt line ->
      let args = Str.split (Str.regexp "\t") line in
      match args with
      |h::i::[] ->
        begin
          if String.length h > 1 then
            failwith "Line contained more than one character in query"
          else
            let res = CharWT.rank wt (String.get h 0) (1 + int_of_string i) in
            match res with
            | Some n -> printf "%d\n" n
            | None -> printf "Not found\n"
        end
      |_ -> failwith "Invalid line"
    ) wt_file query_file

let select wt_file query_file =
  iter_lines (fun wt line ->
      let args = (Str.split (Str.regexp "\t") line) in
      match args with
      |h::i::[] ->
        begin
          if String.length h > 1 then
            failwith "Line contained more than one character in query"
          else
            let res = CharWT.select wt (String.get h 0) (int_of_string i) in
            match res with
            | Some n -> printf "%d\n" @@ n - 1
            | None -> printf "Not found\n"
        end
      |_ -> failwith "Invalid line"
    ) wt_file query_file

let () = if Array.length Sys.argv != 4 then printf "%s" usage_msg
         else
           (match Sys.argv.(1) with
            | "build" -> build
            | "access" -> access
            | "rank" -> rank
            | "select" -> select
            | _ -> failwith "Invalid command") Sys.argv.(2) Sys.argv.(3) 
