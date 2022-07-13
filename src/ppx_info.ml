open Migrate_parsetree
open Ast_410
open Parsetree
open Ast_mapper
open Ast_helper

let build_annot record : Ast_410.Parsetree.attribute =
  (* todo a completer *)
  Attr.mk (Location.mknoloc "infos") (PStr [ Str.eval record ])

let compile (infos : Collector.declaration_result) =
  Exp.record
    [
      ( { txt = Lident "declaration_name"; loc = !default_loc },
        Exp.constant (Pconst_string (infos.declaration_name, None)) );
      ( { txt = Lident "declaration_size"; loc = !default_loc },
        Exp.constant
          (Pconst_integer (string_of_int infos.declaration_size, None)) );
      ( { txt = Lident "collected_fun_calls"; loc = !default_loc },
        Exp.array
          (List.map
             (fun s -> Exp.constant (Pconst_integer (s, None)))
             (Collector.StringSet.elements infos.collected_fun_calls)) );
      ( { txt = Lident "collected_constants_integer"; loc = !default_loc },
        Exp.array
          (List.map
             (fun s -> Exp.constant (Pconst_integer (s, None)))
             (Collector.StringSet.elements infos.collected_constants_integer))
      );
      ( { txt = Lident "collected_constants_char"; loc = !default_loc },
        Exp.array
          (List.map
             (fun s -> Exp.constant (Pconst_integer (s, None)))
             (Collector.StringSet.elements infos.collected_constants_char)) );
      ( { txt = Lident "collected_constants_string"; loc = !default_loc },
        Exp.array
          (List.map
             (fun s -> Exp.constant (Pconst_integer (s, None)))
             (Collector.StringSet.elements infos.collected_constants_string)) );
      ( { txt = Lident "collected_constants_float"; loc = !default_loc },
        Exp.array
          (List.map
             (fun s -> Exp.constant (Pconst_integer (s, None)))
             (Collector.StringSet.elements infos.collected_constants_float)) );
      ( { txt = Lident "potential_exception"; loc = !default_loc },
        Exp.constant
          (Pconst_string (Bool.to_string infos.potential_exception, None)) );
      ( { txt = Lident "potential_catch_exception"; loc = !default_loc },
        Exp.constant
          (Pconst_string (Bool.to_string infos.potential_catch_exception, None))
      );
      ( { txt = Lident "declaration_changed"; loc = !default_loc },
        Exp.constant
          (Pconst_string (Bool.to_string infos.declaration_changed, None)) );
    ]
    None

(* actual mapper : Parsetree -> Parsetree *)
let mapper =
  let handle_bind _mapper (bind : Ast_410.Parsetree.value_binding) =
    let infos = Collector.work_record bind in
    let record = compile infos in
    { bind with pvb_attributes = build_annot record :: bind.pvb_attributes }
  in
  { default_mapper with value_binding = handle_bind }

let () =
  let open Migrate_parsetree in
  Driver.register ~name:"ppx_info" ~args:[] Versions.ocaml_410
    (fun _config _cookies -> mapper)
