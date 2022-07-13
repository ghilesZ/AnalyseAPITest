let () =
  if Array.length Sys.argv <> 2 then failwith "I was expecting a .ml file"
  else if Filename.extension Sys.argv.(1) <> ".ml" then
    failwith "I was expecting a .ml file"
  else
    let filename = Sys.argv.(1) in
    (*Format.printf "Parsing file : %s \n" filename;*)
    let structure = Pparse.parse_implementation ~tool_name:"" filename in
    (*Format.printf "@.%a\n\n" Pprintast.structure structure;*)
    let result =
      Collector.work filename (Collector.DeConv.copy_structure structure)
    in
    Format.printf "%a@." Collector.pp_collection_result result

let () =
  let diff = Diff.diff Sys.argv.(1) in
  Format.printf "This file has changed : %b\n" (Diff.has_change diff)
