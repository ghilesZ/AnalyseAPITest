open Migrate_parsetree
open Ast_410
open Parsetree
module StringSet = Set.Make (String)
module Conv = Convert (OCaml_410) (OCaml_current)
module DeConv = Convert (OCaml_current) (OCaml_410)

let print_expression fmt e =
  Format.fprintf fmt "%a%!" Pprintast.expression (Conv.copy_expression e)

let print_pat fmt p =
  Format.fprintf fmt "%a%!" Pprintast.pattern (Conv.copy_pattern p)

type declaration_result = {
  declaration_name : string;
  declaration_size : int;
  collected_fun_calls : StringSet.t;
  collected_constants_integer : StringSet.t;
  collected_constants_char : StringSet.t;
  collected_constants_string : StringSet.t;
  collected_constants_float : StringSet.t;
  potential_exception : bool;
  potential_catch_exception : bool;
  declaration_changed : bool;
}

module ComparableDeclarationResult = struct
  type t = declaration_result

  let compare (e : declaration_result) (e' : declaration_result) : int =
    String.compare e.declaration_name e'.declaration_name
end

module DeclarationSet = Set.Make (ComparableDeclarationResult)

type collection_result = {
  filename : string;
  collection_results : DeclarationSet.t;
}

let pp_collection_results fmt
    {
      declaration_name;
      declaration_size;
      collected_fun_calls;
      collected_constants_integer;
      collected_constants_char;
      collected_constants_string;
      collected_constants_float;
      potential_exception;
      potential_catch_exception;
      declaration_changed;
    } =
  let open Format in
  let pp_string_set fmt set =
    let l = StringSet.elements set in
    if l = [] then fprintf fmt "∅"
    else
      fprintf fmt "{@[<h>%a@]}"
        (pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt ", ")
           (fun fmt s -> fprintf fmt "'%s'" s))
        l
  in
  fprintf fmt
    "@[<v 2>Name: '%s'@ Structure size: %d@ Function calls: %a@ Integers: %a@ \
     Characters: %a@ Strings: %a@ Floats: %a@ Potential exception: %b@ \
     Potential catch exception: %b@ Declaration changed: %b@]"
    declaration_name declaration_size pp_string_set collected_fun_calls
    pp_string_set collected_constants_integer pp_string_set
    collected_constants_char pp_string_set collected_constants_string
    pp_string_set collected_constants_float potential_exception
    potential_catch_exception declaration_changed

let pp_collection_result fmt { filename; collection_results } =
  let open Format in
  fprintf fmt "@[<v 2>Result for '%s'@ %a@]" filename
    (pp_print_list pp_collection_results)
    (DeclarationSet.elements collection_results)

(**returns the lexical length of a structure using its location*)
let length_lex (loc : Location.t) : int =
  let open Lexing in
  let start_loc = loc.loc_start.pos_lnum in
  let end_loc = loc.loc_end.pos_lnum in
  end_loc - start_loc + 1

(**returns true if the function has been modified, else false
    (if there is an intersection between the location and the files changes)*)
let differ (location : Warnings.loc) : bool =
  let diff = Diff.diff Sys.argv.(1) in
  let modif_bool_list =
    List.map (fun dif -> Diff.intersect dif location) diff.modifs
  in
  List.mem true modif_bool_list

let collect_int (constant_list : constant list) : string list =
  List.flatten
    (List.map
       (fun constant ->
         match constant with
         | Pconst_integer (prefixe, char_op) ->
             [
               (prefixe
               ^ match char_op with Some c -> String.make 1 c | None -> "");
             ]
         | _ -> [])
       constant_list)

let collect_char (constant_list : constant list) : string list =
  List.flatten
    (List.map
       (fun constant ->
         match constant with
         | Pconst_char character -> [ String.make 1 character ]
         | _ -> [])
       constant_list)

let collect_string (constant_list : constant list) : string list =
  List.flatten
    (List.map
       (fun constant ->
         match constant with
         | Pconst_string (prefixe, _) -> [ prefixe ]
         | _ -> [])
       constant_list)

let collect_float (constant_list : constant list) : string list =
  List.flatten
    (List.map
       (fun constant ->
         match constant with
         | Pconst_float (prefixe, char_op) ->
             [
               (prefixe
               ^ match char_op with Some c -> String.make 1 c | None -> "");
             ]
         | _ -> [])
       constant_list)

(** Transforms a ('a * expression) list to expression list
    e.g : [(l1, E1) ; ... ; (ln, En)] en [E1 ; ... ; En]*)
let list_of_expr list : expression list = List.map snd list

(** Transforms case list to expresssion list (the option part)) *)
let rec case_option (list : case list) : expression list =
  match list with
  | [] -> []
  | case :: tail -> (
      match case.pc_guard with
      | None -> case_option tail
      | Some e -> e :: case_option tail)

(*Transforms case list into expression list*)
let case_expression (list : case list) : expression list =
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
        list1 @ list2
        @ Option.value (Option.map traverse branch_else) ~default:[]
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
    | Pexp_function cases ->
        let list1 = case_option cases in
        let list2 = case_expression cases in
        List.flatten (List.map traverse list1)
        @ List.flatten (List.map traverse list2)
    | Pexp_variant (_, option) ->
        Option.value (Option.map traverse option) ~default:[]
    | Pexp_record (tuple_list, option) ->
        let list1 = list_of_expr tuple_list in
        List.flatten (List.map traverse list1)
        @ Option.value (Option.map traverse option) ~default:[]
    | Pexp_field (expression, _) -> traverse expression
    | Pexp_setfield (expression1, _, expression2) ->
        traverse expression1 @ traverse expression2
    | Pexp_array list -> List.flatten (List.map traverse list)
    | Pexp_for (_, init, n, _, body) ->
        traverse init @ traverse n @ traverse body
    | Pexp_constraint (expression, _) -> traverse expression
    | Pexp_coerce (expression, _, _) -> traverse expression
    | Pexp_send (expression, _) -> traverse expression
    | Pexp_new _ -> []
    | Pexp_setinstvar (_, expression) -> traverse expression
    | Pexp_override list -> List.flatten (List.map traverse (list_of_expr list))
    | Pexp_letmodule (_, mod_expr, module_body) ->
        let desc = mod_expr.pmod_desc in
        (match desc with Pmod_unpack expr -> traverse expr | _ -> [])
        @ traverse module_body
    | Pexp_letexception (_, body) -> traverse body
    | Pexp_assert assert_expr -> traverse assert_expr
    | Pexp_lazy lazy_expr -> traverse lazy_expr
    | Pexp_poly (poly_expr, _) -> traverse poly_expr
    | Pexp_object _ -> []
    | Pexp_newtype (_, new_expr) -> traverse new_expr
    | Pexp_pack mod_expr -> (
        let desc = mod_expr.pmod_desc in
        match desc with Pmod_unpack expr -> traverse expr | _ -> [])
    | Pexp_open (_open_dec, open_expr) -> traverse open_expr
    | Pexp_letop letop_expr -> traverse letop_expr.body
    | Pexp_extension _ -> []
    | Pexp_unreachable ->
        Format.printf "%a pas encore implémenté - on saute" print_expression
          expression;
        []
  in
  traverse expression

(* Returns true if raises exception else false*)
let fun_raise_exception (list : Longident.t list) : bool =
  List.exists
    (fun lid -> List.mem lid [ "raise"; "failwith"; "invalid_arg" ])
    (List.flatten (List.map Longident.flatten list))

let fun_catch_exception (expression : expression) : bool =
  match expression.pexp_desc with Pexp_try (_, _) -> true | _ -> false

let work_record (bind : value_binding) : declaration_result =
  let collect_fun_calls = function
    | Pexp_apply ({ pexp_desc = Pexp_ident lid; _ }, _) -> [ lid.txt ]
    | _ -> []
  in
  let list_fun_call = collect collect_fun_calls bind.pvb_expr in

  let collect_constant = function
    | Pexp_constant constant -> [ constant ]
    | _ -> []
  in
  let list_constant = collect collect_constant bind.pvb_expr in

  let collected_fun_calls =
    StringSet.of_list (List.flatten (List.map Longident.flatten list_fun_call))
  in

  let collected_constants_integer =
    StringSet.of_list (collect_int list_constant)
  in

  let collected_constants_char =
    StringSet.of_list (collect_char list_constant)
  in

  let collected_constants_string =
    StringSet.of_list (collect_string list_constant)
  in

  let collected_constants_float =
    StringSet.of_list (collect_float list_constant)
  in
  let declaration_name = Format.asprintf "%a" print_pat bind.pvb_pat in
  {
    declaration_name;
    declaration_size = length_lex bind.pvb_loc;
    collected_fun_calls;
    collected_constants_integer;
    collected_constants_char;
    collected_constants_string;
    collected_constants_float;
    potential_exception = fun_raise_exception list_fun_call;
    potential_catch_exception = fun_catch_exception bind.pvb_expr;
    declaration_changed = differ bind.pvb_loc;
  }

let work_struct (str : structure_item) =
  match str.pstr_desc with
  | Pstr_value (_rec_flag, bindings) -> Some (List.map work_record bindings)
  | _ -> None

let work filename (structure : Parsetree.structure_item list) :
    collection_result =
  let collection_results =
    DeclarationSet.of_list
      (List.flatten (List.filter_map work_struct structure))
  in
  { filename; collection_results }
