open Parsetree

(** Transforme la liste de labels et expression en list d'expression seulement
du genre: [(l1, E1) ; ... ; (ln, En)] en [E1 ; ... ; En]*)
let rec list_of_expr list = match list with 
    | [] -> []
    | (l,e) :: tail -> e :: list_of_expr tail


(* Trouver les constantes dans expression de value_binding et les met dans un tableau *)
let rec work_expr expression  =
    match expression.pexp_desc with
    | Pexp_constant (constant) -> [constant] 
    | Pexp_ifthenelse (expr1, expr2, option) -> let list1 = work_expr(expr1) in 
       let list2 = work_expr expr2 in
       (match option with 
       |None -> list1 @ list2
       |Some(e) -> list1@list2@ work_expr e ) 
    |Pexp_fun (_, option, _, expr) -> let list1 = work_expr expr in
        (match option with 
        |None -> list1
        |Some(e) -> work_expr e @ list1) 
    | Pexp_apply (expr1, list) -> let list1 = work_expr expr1 in 
        let list2 = list_of_expr list in 
        list1 @ List.iter work_expr list2
    | Pexp_let (_, list, expr) -> let list1 = List.iter work_expr (list_of_expr list) in
        list1 @ work_expr expr
    | _ -> invalid_arg "Pas encore implémenté"
    

let work_binding (bind : value_binding) =
    Format.printf "pattern : %a\n" Pprintast.pattern bind.pvb_pat;
    (*Format.printf "expression : %a\n" Pprintast.expression bind.pvb_expr*)
    let list_constant = work_expr bind.pvb_expr in
    List.iter (fun c -> Format.printf "%a\n" Pprintast.expression (Ast_helper.Exp.constant c)) list_constant


let work_struct str =
  match str.pstr_desc with
  | Pstr_value (_rec_flag, bindings) -> List.iter work_binding bindings (*a modifier *)
  | _ -> ()

let work structure =
  Format.printf "number of structures: %i\n%!" (List.length structure);
  List.iter work_struct structure

let () = 
    if Array.length Sys.argv <> 2 then 
        failwith "I was expecting a .ml file"
    else 
        if Filename.extension Sys.argv.(1) <> ".ml" then
            failwith "I was expecting a .ml file"
        else 
            let filename = Sys.argv.(1) in 
            (*Format.printf "Parsing file : %s \n" filename;*)
            let structure = Pparse.parse_implementation ~tool_name:"" filename in 
            (*Format.printf "@.%a\n\n" Pprintast.structure structure;*)
            work structure;