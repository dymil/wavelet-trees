(** Command-line program which supports serializing and deserializing
    wavelet trees over a character alphabet. *)
open Printf

let usage_msg = "Usage: wt cmd arg1 arg2\nCommands available:\nbuild <input string> <output file>\naccess <saved wt> <access indices>\nwhere <access indices> is a file containing the indices, each 0-indexed on a separate line\nrank <saved wt> <rank queries>\nwhere <rank queries> is a file containing the queries of format <c>\t<i> where i is 0-indexed\nselect <saved wt> <select queries>\nwhere select queries is of the form of rank queries above"

let build s ofile = ()

let access wt_file indices_file = ()

let rank wt_file query_file = ()

let select wt_file query_file = ()

let () = if Array.length Sys.argv != 4 then printf usage_msg
         else
           (match Sys.argv.(1) with
            | "build" -> build
            | "access" -> access
            | "rank" -> rank
            | "select" -> select) Sys.argv.(2) Sys.argv.(3)

  
