let () =
  if Array.length Sys.argv <> 2 then failwith "I was expecting a .ml file"
  else if Filename.extension Sys.argv.(1) <> ".ml" then
    failwith "I was expecting a .ml file"
  else
    let filename = Sys.argv.(1) in
    (*Format.printf "Parsing file : %s \n" filename;*)
    let structure = Pparse.parse_implementation ~tool_name:"" filename in
    (*Format.printf "@.%a\n\n" Pprintast.structure structure;*)
    Collector.work structure

let () = 
  let diff = Diff.diff Sys.argv.(1) in
  Format.printf "diff %b\n" (Diff.has_change diff)