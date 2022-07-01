(* Small wrapper around system commands *)

(* exec a command and ignores its return status *)
let exec cmd = ignore (Sys.command cmd)

(* launches a command with a list of arguments *)
let run ?(opt = []) cmd args =
  let cmdopt = List.fold_left (fun acc o -> acc ^ " -" ^ o) cmd opt in
  let cmdargs = List.fold_left (fun acc a -> acc ^ " " ^ a) cmdopt args in
  exec cmdargs

(* launches a command with a list of arguments and gets its output *)
let run_output ?opt cmd args =
  let tmp_file = Filename.temp_file "" ".txt" in
  run ?opt cmd (args @ [ ">" ^ tmp_file ]);
  let ch = open_in tmp_file in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

(* calls diff on the given filename and gets the output*)
let diff filename =
  Format.printf "calling diff on %s@." filename;
  let output_diff =
    run_output
      ("git diff -U0 " ^ filename
     ^ " | grep -Po '^\\+\\+\\+ ./\\K.*|^@@ -[0-9]+(,[0-9]+)? \
        \\+\\K[0-9]+(,[0-9]+)?(?= @@)'")
      []
  in
  output_diff
