open Migrate_parsetree
open Ast_410
open Parsetree
open Ast_mapper
open Ast_helper

let build_annot () =
  (* todo a completer *)
  Attr.mk (Location.mknoloc "infos") (PStr [])

(* let _compile infos =
 *   Exp.record *)

(* actual mapper : Parsetree -> Parsetree *)
let mapper =
  let handle_bind _mapper (bind : value_binding) =
    (* let _infos = Collector.work_binding bind in
       let record = compile infos in
    *)
    let record = () in
    { bind with pvb_attributes = build_annot record :: bind.pvb_attributes }
  in
  { default_mapper with value_binding = handle_bind }

let () =
  let open Migrate_parsetree in
  Driver.register ~name:"ppx_info" ~args:[] Versions.ocaml_410
    (fun _config _cookies -> mapper)
