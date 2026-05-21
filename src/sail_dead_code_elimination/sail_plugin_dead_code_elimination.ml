open Libsail
open Interactive.State
open Ast
open Ast_util
open Ast_compare
open Jib
open Jib_util
open Value2
open Printf 
open Dc_env

let opt_output_dir = ref (None)
let opt_assumptions_dir = ref (None)

let get_variable_and_literal (line : string) :  (string * string) option = 
    let parts = String.split_on_char '=' line in
    if List.length parts = 2 then
      let variable = String.trim (List.nth parts 0) in
      let literal = String.trim (List.nth parts 1) in
      Some (variable, literal)
    else
      None


let opt_output_file = ref (None)
let dead_code_elimination_options =
  [
    ( Flag.create ~prefix:["dead_code_elimination"] ~arg:"directory" "output_dir",
      Arg.String (fun dir -> opt_output_dir := Some dir),
      "set a custom directory for dead code elimination output"
    );
    ( Flag.create ~prefix:["dead_code_elimination"] ~arg: "assumptions" "assumptions_file",
      Arg.String (fun dir -> opt_assumptions_dir := Some dir),
      "What assumptions to include for elimination" 
    );
    ( Flag.create ~prefix:["dead_code_elimination"] ~arg: "output" "output_file",
      Arg.String (fun file -> opt_output_file := Some file),
      "set a custom output file for dead code elimination results"
    );
  ]

let is_boolean_binop id =
  let op = string_of_id id in
  match op with
    | "&&" | "||" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "gt_int" | "lt_int" | "lteq_int" | "gteq_int" | "eq_int" | "neq_int"
    | "eq_bool" | "neq_bool" | "eq_string" -> true
    | _ -> false

let rec evaluate_boolean_expr (expr : 'a exp) (dc_env : dc_env) : string option =
  Printf.printf "Evaluating expression: %s\n" (string_of_exp expr);
  match expr with
  | E_aux (E_app (id, args), _) when is_boolean_binop id ->
    let evaluated_args = List.map (fun arg -> evaluate_boolean_expr arg dc_env) args in
    if List.exists (fun arg -> arg = None) evaluated_args then (Printf.printf "Could not evaluate all arguments\n";None)
    else
      if List.length evaluated_args = 2 then(
        Printf.printf "Evaluating binary operator %s with arguments %s and %s\n" (string_of_id id) (Option.value (List.nth evaluated_args 0) ~default:"None") (Option.value (List.nth evaluated_args 1) ~default:"None"); 
        let arg1 = List.nth evaluated_args 0 in
        let arg2 = List.nth evaluated_args 1 in
        (match string_of_id id with
          | "&&" -> (match (arg1, arg2) with
            | (Some "true", Some "true") -> Some "true"
            | (Some "false", _) | (_, Some "false") -> Some "false"
            | _ -> None)
          | "||" -> (match (arg1, arg2) with
            | (Some "false", Some "false") -> Some "false"
            | (Some "true", _) | (_, Some "true") -> Some "true"
            | _ -> None)
          | "==" | "eq_string" -> 
              print_endline ("Comparing " ^ (Option.value arg1 ~default:"None") ^ " and " ^ (Option.value arg2 ~default:"None"));
              if arg1 = arg2 then Some "true" else Some "false"
          | "!=" -> if arg1 <> arg2 then Some "true" else Some "false"
          | "<" -> (match (arg1, arg2) with
            | (Some a, Some b) when (int_of_string a) < (int_of_string b) -> Some "true"
            | (Some a, Some b) when (int_of_string a) >= (int_of_string b) -> Some "false"
            | _ -> None)
          | "gt_int" -> (match (arg1, arg2) with
            | (Some a, Some b) when (int_of_string a) > (int_of_string b) -> Some "true"
            | (Some a, Some b) when (int_of_string a) <= (int_of_string b) -> Some "false"
            | _ -> None)
          | "<=" -> (match (arg1, arg2) with
            | (Some a, Some b) when (int_of_string a) <= (int_of_string b) -> Some "true"
            | (Some a, Some b) when (int_of_string a) > (int_of_string b) -> Some "false"
            | _ -> None)
          | ">=" -> (match (arg1, arg2) with
            | (Some a, Some b) when (int_of_string a) >= (int_of_string b) -> Some "true"
            | (Some a, Some b) when (int_of_string a) < (int_of_string b) -> Some "false"
            | _ -> None)
            | _ -> None))
      else if List.length evaluated_args = 1 then
        let arg = List.nth evaluated_args 0 in
        (match string_of_id id with
          | "!" -> (match arg with
            | Some "true" -> Some "false"
            | Some "false" -> Some "true"
            | _ -> None)
          | _ -> None)
      else None
  | E_aux (E_lit lit, _) -> (match lit with
      | L_aux (L_num n, _) -> (Some (Int.to_string (Big_int.to_int n)))
      | L_aux (L_string s, _) -> Some s
      | L_aux (L_true, _) -> Some "true"
      | L_aux (L_false, _) -> Some "false"
      | _ -> None)
  | E_aux (E_id id, _) -> (find_variable_opt (string_of_id id) dc_env)
  | _ -> 
    Printf.printf "Expression type not supported for evaluation: %s\n" (string_of_exp expr);
    None

let rec check_expr (env : Type_check.env) (exp : 'a exp) (dc_env : dc_env) : 'a exp =
  match exp with
  | E_aux (E_block exps, dummy) -> 
    let checked_exps = (List.fold_left (fun acc exp ->
      let checked_expression = check_expr env exp dc_env in
      match checked_expression with
        | E_aux (E_lit (L_aux (L_unit, Parse_ast.Unknown)), annot) -> acc
        | _ -> acc @ [checked_expression]) [] exps) in
      (E_aux (E_block checked_exps, dummy))
  | E_aux (E_let (pat, exps, body), dummy) -> 
    (E_aux (E_let (pat, exps, check_expr env body dc_env), dummy))
  | E_aux (E_if (cond, then_exp, else_exp), dummy) ->
    let cond_val = evaluate_boolean_expr cond dc_env in
    (match cond_val with
    | Some ("true") ->
      Printf.printf "Condition evaluated to true, eliminating else branch\n"; 
      check_expr env then_exp dc_env
    | Some ("false") -> 
      Printf.printf "Condition evaluated to false, eliminating then branch\n"; 
      check_expr env else_exp dc_env
    | None -> 
      Printf.printf "Condition could not be evaluated, checking both branches\n";
      (E_aux (E_if (cond, check_expr env then_exp dc_env, check_expr env else_exp dc_env), dummy))
    | _ -> raise (Failure "Condition expression did not evaluate to a boolean literal or undetermined value")    
    )
  | E_aux (E_return e, d) ->
    (E_aux (E_return (check_expr env e dc_env), d))
  | E_aux (E_lit (L_aux (L_unit, Parse_ast.Unknown)), annot) ->
      E_aux (E_lit (L_aux (L_unit, Parse_ast.Unknown)), annot)
  | E_aux (E_lit lit, dummy) ->
      (E_aux (E_lit lit, dummy))
  | E_aux (E_app (id, args), dummy) -> (*Function call *)
      E_aux (E_app (id, args), dummy)
  | E_aux (E_id id, _) ->
    raise (Failure "Variable references not supported in dead code elimination")
  | _ -> raise (Failure "Expression type not supported in dead code elimination")


let is_named_function_val_spec val_spec =
  match val_spec with
  | VS_aux (VS_val_spec (TypSchm_aux (TypSchm_ts (_, Typ_aux (Typ_fn (_, _), _)), _), id, _), _) ->
      let name = string_of_id id in
      name = "main" || name = "foo"
  | _ -> false


let check_ast (env : Type_check.env) (ast : Type_check.typed_ast) (dc_env : dc_env) : Type_check.typed_ast = 
  let output_ast = List.fold_left (fun acc def -> 
    match def with
    | DEF_aux (DEF_val val_spec, _) ->
        acc @ [def]
    | DEF_aux (DEF_fundef (FD_aux (FD_function (d1, d2, funcls), d3)), d4) ->
        (let func_ast = (List.fold_left 
          (fun acc (FCL_aux (FCL_funcl (id, pexp), d5)) ->
            Printf.printf "Checking function %s for dead code\n" (string_of_id id);
            match pexp with
            |Pat_aux (Pat_exp (input, body), d6) -> 
              let checked_ast = check_expr env body dc_env in
              acc @ [DEF_aux (DEF_fundef (FD_aux (FD_function (d1, d2, [(FCL_aux (FCL_funcl (id, Pat_aux (Pat_exp (input, checked_ast), d6)), d5))]), d3)), d4)]
            | _ -> acc))
          [] funcls in
          acc @ func_ast)
    | _ -> acc
  ) [] ast.defs in
  { ast with defs = output_ast}

let defs_without_includes defs =
    let rec go depth acc = function
      | DEF_aux (DEF_pragma ("include_start", _), _) :: rest -> go (depth + 1) acc rest
      | DEF_aux (DEF_pragma ("include_end", _), _) :: rest -> go (max 0 (depth - 1)) acc rest
      | def :: rest when depth = 0 -> go depth (def :: acc) rest
      | _ :: rest -> go depth acc rest
      | [] -> List.rev acc
    in
    go 0 [] defs

  
let dead_code_elimination_target out_file { ast; effect_info; env; _ } =
  let output_filename = match !opt_output_file with
    | Some file -> file
    | None -> raise (Arg.Bad "Output file must be specified with --dead-code-elimination-output")
    in
  let assumptions_filename = match !opt_assumptions_dir with
    | Some file -> file
    | None -> raise (Arg.Bad "Assumptions file must be specified with --dead-code-elimination-assumptions")
    in
  let output_dir = match !opt_output_dir with
    | Some dir -> dir
    | None -> "./test/non_interference/dead_code_output"
    in
  let open Ast in
  let open Ast_defs in
  
  let dc_env = ref empty_dc_env in
  let read_ass = open_in assumptions_filename in
  (try
    while true do
      let line = input_line read_ass in
      dc_env := match get_variable_and_literal line with 
        | Some (v, l) -> add_variable v l !dc_env
        | None -> !dc_env
    done
  with
  | End_of_file -> close_in read_ass
  | e -> close_in_noerr read_ass; raise e);

    let read_ass = open_in assumptions_filename in
    (try
    while true do
      let line = input_line read_ass in
      match get_variable_and_literal line with
        | Some (v, _) -> print_endline ("v is " ^ (Option.value (find_variable_opt v !dc_env) ~default:"Variable not found"))
        | None -> print_endline (Option.value (find_variable_opt line !dc_env) ~default:"Variable not found when parsing empty line")
    done
    with
    | End_of_file -> close_in read_ass
    | e -> close_in_noerr read_ass; raise e);


  let user_defs = defs_without_includes ast.defs in
  


  let output_ast = check_ast env { ast with defs = user_defs } !dc_env in
  let temp_dir = (output_dir ^  "/temp/" ^ output_filename) in
  Printf.printf "Checked AST beginning printing\n";
  let chan = open_out temp_dir in
  let stripped = Type_check.strip_ast output_ast in
  Pretty_print_sail.output_ast chan stripped;
  close_out chan;
  flush_all ();

  let read_temp_chan = open_in temp_dir in
  let write_chan = open_out (output_dir ^ "/" ^output_filename ^ ".sail") in
  (try
    output_string write_chan "default Order dec\n$include <prelude.sail>\n\n";
    while true do
      let line = (input_line read_temp_chan) ^ "\n" in
      output_string write_chan line
    done
  with
  | End_of_file -> close_in read_temp_chan
  | e -> close_in_noerr read_temp_chan; raise e);
  close_in read_temp_chan;
  close_out write_chan;
  flush_all ();
  Printf.printf "Finished printing\n";
  ()

let _ =
  Target.register
    ~name:"dead_code_elimination"
    ~options:dead_code_elimination_options
    ~rewrites:[]
    dead_code_elimination_target