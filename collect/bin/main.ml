open Parsetree

let work_binding (bind : value_binding) =
  Format.printf "pattern : %a\n" Pprintast.pattern bind.pvb_pat;
  Format.printf "expression : %a\n" Pprintast.expression bind.pvb_expr

let cpt = ref 0

let work_struct str =
  match str.pstr_desc with
  | Pstr_value (_rec_flag, bindings) -> cpt := !cpt + 1; List.iter work_binding bindings
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
      work structure;
      Format.printf "number of pstr_value : %i\n" !cpt
