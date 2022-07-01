(* Small wrapper around system commands *)

(* exec a command and ignores its return status *)
let exec cmd = ignore (Sys.command cmd)

(* launches a command with a list of arguments *)
let run ?(opt = []) cmd args =
  let cmdopt = List.fold_left (fun acc o -> acc ^ " -" ^ o) cmd opt in
  let cmdargs = List.fold_left (fun acc a -> acc ^ " " ^ a) cmdopt args in
  exec cmdargs

let lines_from_files filename =
  let ch_in = open_in filename in
  let rec lines_from_files_aux i acc =
    match input_line i with
    | s -> lines_from_files_aux i (s :: acc)
    | exception End_of_file -> close_in ch_in ; List.rev acc
  in
  lines_from_files_aux ch_in []

(* launches a command with a list of arguments and gets its output *)
let run_output ?opt cmd args =
  let tmp_file = Filename.temp_file "" ".txt" in
  run ?opt cmd (args @ [ ">" ^ tmp_file ]);
  let s = lines_from_files tmp_file in 
  s

(* calls diff on the given filename and gets the output*)
let diff filename =
  let output_diff =
    run_output
      ("git diff -U0 " ^ filename
     ^ " | grep -Po '^\\+\\+\\+ ./\\K.*|^@@ -[0-9]+(,[0-9]+)? \
        \\+\\K[0-9]+(,[0-9]+)?(?= @@)'")
      []
  in
  List.tl output_diff
