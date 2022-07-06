(* Small wrapper around system commands *)

(* exec a command and ignores its return status *)
let exec cmd = Unix.open_process_in cmd

(* launches a command with a list of arguments *)
let run ?(opt = []) cmd args =
  let cmdopt = List.fold_left (fun acc o -> acc ^ " -" ^ o) cmd opt in
  let cmdargs = List.fold_left (fun acc a -> acc ^ " " ^ a) cmdopt args in
  exec cmdargs

(* launches a command with a list of arguments and gets its output *)
let run_output ?opt cmd args =
  let ic = run ?opt cmd args in
  let rec loop acc =
    match input_line ic with
    | s -> loop (s :: acc)
    | exception End_of_file -> acc
  in
  List.rev (loop [])

  type t = {
    filename : string;
    modifs : (int * int) list;
  }

let has_change diff =
  diff.modifs <> []  

(** returns true if theres an intersection between an interval (int * int) and a location *)
let intersect ((a : int), (b : int)) (loc : Location.t): bool =
  let open Lexing in 
  let start_loc = loc.loc_start.pos_lnum in
  let end_loc = loc.loc_end.pos_lnum in 
  (a <= start_loc && b>= start_loc) || (a <= end_loc && b >= end_loc) || (a <= start_loc && b >= end_loc) || (a >= start_loc && b <= end_loc)

(* calls diff on the given filename and gets the output*)
let diff filename =
  let output_diff =
    run_output
      ("git diff -U0 " ^ filename
     ^ " | grep -Po '^\\+\\+\\+ ./\\K.*|^@@ -[0-9]+(,[0-9]+)? \
        \\+\\K[0-9]+(,[0-9]+)?(?= @@)'")
      []
  in
  let parse str = (match String.split_on_char ',' str with
    |[a;b] -> ( int_of_string a, int_of_string a + int_of_string b)
    |[a] -> (int_of_string a, int_of_string a)
    | _ -> failwith "parse error") in

  let filename = List.hd output_diff in
  let modifs = List.map parse (List.tl output_diff) in
   {filename = filename; modifs = modifs}
  





