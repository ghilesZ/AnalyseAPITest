open Parsetree

(** Transforme la liste de labels et expression en liste d'expression seulement
du genre: [(l1, E1) ; ... ; (ln, En)] en [E1 ; ... ; En]*)
let rec list_of_expr list : expression list =
  match list with [] -> [] | (_, e) :: tail -> e :: list_of_expr tail

(*Transforme la "case list" en liste d'expression seulement (partie option)*)
let rec case_option list : expression list =
  match list with
  | [] -> []
  | case :: tail -> (
      match case.pc_guard with
      | None -> case_option tail
      | Some e -> e :: case_option tail)

(*Transforme la "case list" en liste d'expression seulement*)
let rec case_expression list : expression list =
  match list with
  | [] -> []
  | case :: tail -> case.pc_rhs :: case_expression tail

(* Trouver les constantes dans expression de value_binding et les met dans un tableau *)
let rec collect_constant expression : constant list =
  match expression.pexp_desc with
  | Pexp_constant constant -> [ constant ]
  | Pexp_ident _ -> []
  | Pexp_ifthenelse (expr1, expr2, option) -> (
      let list1 = collect_constant expr1 in
      let list2 = collect_constant expr2 in
      match option with
      | None -> list1 @ list2
      | Some e -> list1 @ list2 @ collect_constant e)
  | Pexp_fun (_, option, _, expr) -> (
      let list1 = collect_constant expr in
      match option with None -> list1 | Some e -> collect_constant e @ list1)
  | Pexp_apply (expr1, list) ->
      let list1 = collect_constant expr1 in
      let list2 = list_of_expr list in
      list1 @ List.flatten (List.map collect_constant list2)
  | Pexp_let (_, list, body) ->
      let expressions = List.map (fun vb -> vb.pvb_expr) list in
      let constant_expr = List.map collect_constant expressions in
      List.flatten constant_expr @ collect_constant body
  | Pexp_tuple list -> List.flatten (List.map collect_constant list)
  | Pexp_sequence (expression1, expression2) ->
      collect_constant expression1 @ collect_constant expression2
  | Pexp_try (expression, cases) ->
      let list1 = collect_constant expression in
      let list2 = case_option cases in
      let list3 = case_expression cases in
      list1
      @ List.flatten (List.map collect_constant list2)
      @ List.flatten (List.map collect_constant list3)
  | Pexp_match (expression, cases) ->
      let list1 = collect_constant expression in
      let list2 = case_option cases in
      let list3 = case_expression cases in
      list1
      @ List.flatten (List.map collect_constant list2)
      @ List.flatten (List.map collect_constant list3)
  | Pexp_construct (_, exp) ->
      List.flatten (List.map collect_constant (Option.to_list exp))
  | _ ->
      (* Format.kasprintf invalid_arg "%a: pas encore implémenté"
         Pprintast.expression expression*)
      Format.printf "%a: pas encore implémenté - on saute" Pprintast.expression
        expression;
      []

let rec collect_fun_calls expression : Longident.t list =
  match expression.pexp_desc with
  | Pexp_constant _ -> []
  | Pexp_ident _ -> []
  | Pexp_ifthenelse (expr1, expr2, option) -> (
      let list1 = collect_fun_calls expr1 in
      let list2 = collect_fun_calls expr2 in
      match option with
      | None -> list1 @ list2
      | Some e -> list1 @ list2 @ collect_fun_calls e)
  | Pexp_fun (_, option, _, expr) -> (
      let list1 = collect_fun_calls expr in
      match option with None -> list1 | Some e -> collect_fun_calls e @ list1)
  | Pexp_apply ({ pexp_desc = Pexp_ident lid; _ }, list) ->
      let list2 = list_of_expr list in
      [ lid.txt ] @ List.flatten (List.map collect_fun_calls list2)
  | Pexp_apply (expr, list) ->
      let list1 = collect_fun_calls expr in
      let list2 = list_of_expr list in
      list1 @ List.flatten (List.map collect_fun_calls list2)
  | Pexp_let (_, list, body) ->
      let expressions = List.map (fun vb -> vb.pvb_expr) list in
      let constant_expr = List.map collect_fun_calls expressions in
      List.flatten constant_expr @ collect_fun_calls body
  | Pexp_tuple list -> List.flatten (List.map collect_fun_calls list)
  | Pexp_sequence (expr1, expr2) ->
      collect_fun_calls expr1 @ collect_fun_calls expr2
  | Pexp_try (expression, cases) ->
      let list1 = collect_fun_calls expression in
      let list2 = case_option cases in
      let list3 = case_expression cases in
      list1
      @ List.flatten (List.map collect_fun_calls list2)
      @ List.flatten (List.map collect_fun_calls list3)
  | Pexp_match (expression, cases) ->
      let list1 = collect_fun_calls expression in
      let list2 = case_option cases in
      let list3 = case_expression cases in
      list1
      @ List.flatten (List.map collect_fun_calls list2)
      @ List.flatten (List.map collect_fun_calls list3)
  | Pexp_construct (_, exp) ->
      List.flatten (List.map collect_fun_calls (Option.to_list exp))
  | _ -> invalid_arg "Pas encore implémenté"

(* Return true if raises exception else false*)
let fun_raise_exception list : bool =
  List.exists
    (fun lid -> lid = "raise")
    (List.flatten (List.map Longident.flatten list))

let work_binding (bind : value_binding) =
  Format.printf "pattern : %a\n" Pprintast.pattern bind.pvb_pat;
  (*Format.printf "expression : %a\n" Pprintast.expression bind.pvb_expr*)
  let list_constant = collect_constant bind.pvb_expr in
  List.iter
    (fun c ->
      Format.printf "constant : %a\n" Pprintast.expression
        (Ast_helper.Exp.constant c))
    list_constant;
  let list_fun_call = collect_fun_calls bind.pvb_expr in
  List.iter
    (fun f -> Format.printf "fun_call : %a\n" Pprintast.longident f)
    list_fun_call;
  Format.printf "raise : %b@." (fun_raise_exception list_fun_call)

let work_struct str =
  match str.pstr_desc with
  | Pstr_value (_rec_flag, bindings) ->
      List.iter work_binding bindings (*a modifier *)
  | _ -> ()
open Lexing
let work structure =
  Format.printf "number of structures: %i\n%!" (List.length structure);
  List.iter work_struct structure

let () =
  if Array.length Sys.argv <> 2 then failwith "I was expecting a .ml file"
  else if Filename.extension Sys.argv.(1) <> ".ml" then
    failwith "I was expecting a .ml file"
  else
    let filename = Sys.argv.(1) in
    (*Format.printf "Parsing file : %s \n" filename;*)
    let structure = Pparse.parse_implementation ~tool_name:"" filename in
    (*Format.printf "@.%a\n\n" Pprintast.structure structure;*)
    work structure
