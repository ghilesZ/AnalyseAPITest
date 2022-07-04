open Parsetree

(** Transforms a ('a * expression) list to expression list 
    e.g : [(l1, E1) ; ... ; (ln, En)] en [E1 ; ... ; En]*)
let list_of_expr list : expression list = List.map snd list

(** Transforms case list to expresssion list (the option part)) *)
let rec case_option list : expression list =
  match list with
  | [] -> []
  | case :: tail -> (
      match case.pc_guard with
      | None -> case_option tail
      | Some e -> e :: case_option tail)

(*Transforms case list into expression list*)
let case_expression list : expression list =
  List.map (fun { pc_rhs; _ } -> pc_rhs) list

(** Goes through the abstract syntaxt tree and calls the function f that decides 
    whether to put what it collected in a list or not, depending on the needs*)
let collect f expression =
  let rec traverse expression =
    f expression.pexp_desc
    @
    match expression.pexp_desc with
    | Pexp_constant _ -> []
    | Pexp_ident _ -> []
    | Pexp_ifthenelse (condition, branch_then, branch_else) ->
        let list1 = traverse condition in
        let list2 = traverse branch_then in
        list1 @ list2 @ Option.value (Option.map traverse branch_else) ~default:[]
    | Pexp_fun (_, option, _, expression) -> (
        let list1 = traverse expression in
        match option with None -> list1 | Some e -> traverse e @ list1)
    | Pexp_apply (expression1, list) ->
        let list1 = traverse expression1 in
        let list2 = list_of_expr list in
        list1 @ List.flatten (List.map traverse list2)
    | Pexp_let (_, list, body) ->
        let expressions = List.map (fun vb -> vb.pvb_expr) list in
        let constant_expr = List.map traverse expressions in
        List.flatten constant_expr @ traverse body
    | Pexp_tuple list -> List.flatten (List.map traverse list)
    | Pexp_sequence (expression1, expression2) ->
        traverse expression1 @ traverse expression2
    | Pexp_try (expression, cases) ->
        let list1 = traverse expression in
        let list2 = case_option cases in
        let list3 = case_expression cases in
        list1
        @ List.flatten (List.map traverse list2)
        @ List.flatten (List.map traverse list3)
    | Pexp_match (expression, cases) ->
        let list1 = traverse expression in
        let list2 = case_option cases in
        let list3 = case_expression cases in
        list1
        @ List.flatten (List.map traverse list2)
        @ List.flatten (List.map traverse list3)
    | Pexp_construct (_, exp) ->
        List.flatten (List.map traverse (Option.to_list exp))
    | Pexp_while (condition, body) -> traverse condition @ traverse body
    | Pexp_function (cases) -> 
        let list1 = case_option cases in
        let list2 = case_expression cases in
        List.flatten (List.map traverse list1)
        @ List.flatten (List.map traverse list2)
    | Pexp_variant (_, option) -> Option.value (Option.map traverse option) ~default:[]
    | Pexp_record (tuple_list, option) ->
        let list1 = (list_of_expr tuple_list) in
        List.flatten (List.map traverse list1) @ Option.value (Option.map traverse option) ~default:[]
    | Pexp_field (expression, _) -> traverse expression
    | Pexp_setfield (expression1, _, expression2) -> traverse expression1 @ traverse expression2
    | Pexp_array (list) -> List.flatten (List.map traverse list)
    | Pexp_for (_, init, n, _, body) ->
        traverse init @ traverse n @ traverse body
    | Pexp_constraint (expression, _) -> traverse expression
    | Pexp_coerce (expression, _, _) -> traverse expression
    | Pexp_send (expression, _) -> traverse expression
    | Pexp_new _ -> []
    | Pexp_setinstvar (_, expression) -> traverse expression
    | Pexp_override (list) -> List.flatten (List.map traverse (list_of_expr list))
    | Pexp_letmodule (_,_,_)
    | Pexp_letexception (_, _)
    | Pexp_assert _ | Pexp_lazy _
    | Pexp_poly (_, _)
    | Pexp_object _
    | Pexp_newtype (_, _)
    | Pexp_pack _
    | Pexp_open (_, _)
    | Pexp_letop _ | Pexp_extension _ | Pexp_unreachable ->
        Format.printf "%a: pas encore implémenté - on saute"
          Pprintast.expression expression;
        []
  in
  traverse expression

(* Returns true if raises exception else false*)
let fun_raise_exception list : bool =
  List.exists
    (fun lid -> lid = "raise")
    (List.flatten (List.map Longident.flatten list))

let work_binding (bind : value_binding) =
  Format.printf "pattern : %a@." Pprintast.pattern bind.pvb_pat;
  (*Format.printf "expression : %a@." Pprintast.expression bind.pvb_expr*)
  let collect_constant = function
    | Pexp_constant constant -> [ constant ]
    | _ -> []
  in
  let list_constant = collect collect_constant bind.pvb_expr in
  List.iter
    (fun c ->
      Format.printf "constant : %a@." Pprintast.expression
        (Ast_helper.Exp.constant c))
    list_constant;
  let collect_fun_calls = function
    | Pexp_apply ({ pexp_desc = Pexp_ident lid; _ }, _) -> [ lid.txt ]
    | _ -> []
  in
  let list_fun_call = collect collect_fun_calls bind.pvb_expr in
  List.iter
    (fun f -> Format.printf "fun_call : %a@." Pprintast.longident f)
    list_fun_call;
  Format.printf "raise : %b@." (fun_raise_exception list_fun_call)

let work_struct str =
  match str.pstr_desc with
  | Pstr_value (_rec_flag, bindings) ->
      List.iter work_binding bindings 
  | _ -> ()

let work structure =
  Format.printf "number of structures: %i@." (List.length structure);
  List.iter work_struct structure
