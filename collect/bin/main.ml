let work_binding _bind = ()

let work_struct str =
  let open Parsetree in
  match str.pstr_desc with
  | Pstr_value (_rec_flag, bindings) -> List.iter work_binding bindings
  | _ -> ()

let work structure =
  Format.printf "number of structures: %i\n%!" (List.length structure);
  List.iter work_struct structure

let () =
  if Array.length Sys.argv <> 2 then
    failwith "I was expecting a filename"
  else
    if Filename.extension Sys.argv.(1) <> ".ml" then
          failwith "I was expecting a .ml file"
    else
      let filename = Sys.argv.(1) in
      Format.printf "Parsing file: %s\n" filename;
      let structure = Pparse.parse_implementation ~tool_name:"" filename in
      Format.printf "@.%a\n\n" Pprintast.structure structure;
      work structure
