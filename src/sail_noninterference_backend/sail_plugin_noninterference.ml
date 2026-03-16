open Libsail
open Interactive.State
open Ast
open Ast_util
open Ast_compare
open Jib
open Jib_util
open Value2
open Printf
open Ni_env

module Callgraph_commands = Callgraph_commands

(* Options for noninterference plugin *)
let opt_output_dir = ref (Some ".")
let opt_verbose = ref false
let noninterference_options =
  [
    ( Flag.create ~prefix:["noninterference"] ~arg:"directory" "output_dir",
      Arg.String (fun dir -> opt_output_dir := Some dir),
      "set a custom directory for noninterference analysis output"
    );
    ( Flag.create ~prefix:["noninterference"] "verbose",
      Arg.Set opt_verbose,
      "enable verbose output for noninterference analysis" 
    );
  ]



let check_variable_lattice (ni_env : ni_env) (id: id) : ni_env = (*The purpose of this function is to update our ni_env with lattice information of each var*)
  let var_name = string_of_id id in
  if String.starts_with ~prefix:"public_" var_name then
    add id Public ni_env
  else if String.starts_with ~prefix:"secret_" var_name then
    add id Secret ni_env
  else
    failwith ("Variable " ^ var_name ^ " does not follow naming convention for public or secret variables")

let is_binop id =
  let op = string_of_id id in
  match op with
  | "+" | "-" | "*" | "/" | "&&" | "||" | "==" | "!=" | "<" | ">" | "<=" | ">=" -> true
  | _ -> false

let rec check_expr (env : Type_check.env) (expr : 'a exp) (ni_env : ni_env) : unit = 
  match expr with
  | E_aux (E_app (id, [e1; e2]), _) when is_binop id ->
      check_expr env e1 ni_env;
      check_expr env e2 ni_env;
      
      (*
      Any operator is treated the same in non-interference analysis since it does not matter
      what the operator is. If we have an expression like "x + y", the operator doesn't affect
      the non-interference properties since what we are really interested in, is whether x or y
      are secret and whether they are assigned to a public or secret variable.
      *)
      
  | E_aux (E_lit lit, _) ->
    (match lit with
    | L_aux (L_num _, _) -> ()
    | L_aux (L_true, _) | L_aux (L_false, _) -> ()
    | L_aux (L_real _, _) -> ()
    | L_aux (L_string _, _) -> ()
    | _ -> failwith "Unsupported literal type")
    (* 
    We match literals here to limit what types of literals we currently support in our non-interference
    analysis. This is more of a safety measure to ensure our analysis doesn't fail because of an 
    unsupported literal type.
    *)
  | E_aux (E_assign (lexp, value), _) ->
    (*
    For assignments, we have different cases to consider for the left hand side. Thus we refer to a 
    helper function check_lexp to handle the different cases. These will matter for our non-interference
    analysis. We allow an assignment to go through in three out of four cases.
    - If lexp and value are both public, then the assignment is fine.
    - If lexp is secret and value is public, then the assignment is fine.
    - If lexp is secret and value is secret, then the assignment is fine.
    - If lexp is public and value is secret, then this is a violation of non-interference and should result in a skip.
    For optimization we should check value first and find out whether it is public. If it is secret, then
    we can skip checking lexp since the assignment is fine.
    *)
      
      check_lexp env lexp ni_env;
      check_expr env value ni_env;
      (* handle assignment *)
  | E_aux (E_id id, _) ->
      (*  *)
      ()
  | E_aux (E_if (cond, then_exp, else_exp), _) ->
      check_expr env cond ni_env;
      check_expr env then_exp ni_env;
      check_expr env else_exp ni_env;
      (* handle if statement *)
  | _ -> ()

and check_lexp (env : Type_check.env) (lexp : 'a lexp) (ni_env : ni_env) : unit =
  match lexp with
  | LE_aux (LE_id id, _) -> 
    (*
    In this more simplistic version of the non-interference analysis, we are not checking whether a 
    variable is secret or public. We expect we know this from the start. Thus we use names to specify
    whether a variable is secret or public (secret_x vs public_x). Therefore, we call a seperate function
    every time we encounter a new variable. This can happen in an assignment, a function parameter, or
    a let binding.
    *)
    ni_env' = check_variable_lattice ni_env id;
    (*If the lexp is public we need to check the value*)
    match find id ni_env' with
    | Public -> ()
    | Secret -> ()

  | LE_aux (LE_deref exp, _) -> check_expr env exp ni_env
  | LE_aux (LE_field (lexp, _), _) -> check_lexp env lexp ni_env
  | _ -> ()

let check_ast (env : Type_check.env) (ast : Type_check.typed_ast) (ni_env : ni_env) = 
  List.iter (fun def -> 
    match def with
    | DEF_aux (DEF_fundef (FD_aux (FD_function (_, _, funcls), _)), _) ->
        List.iter 
          (fun (FCL_aux (FCL_funcl (_, pexp), _)) ->
            match pexp with
            |Pat_aux (Pat_exp (_, body), _) -> check_expr env body ni_env
            | _ -> ())
            funcls
    | _ -> ()
  ) ast.defs  

let noninterference_target out_file { ast; effect_info; env; _ } =
  (* Handle optional output file *)
  let output_filename = match out_file with 
    | Some f -> f ^ ".noninterference" 
    | None -> "noninterference_analysis" 
  in
  let open Ast in
  let open Ast_defs in
  
  (* Now you have access to:
     - ast: the AST of the program
     - effect_info: effect information
     - env: the type environment
     - output_filename: output file path *)
  
  if !opt_verbose then (
    Printf.printf "Noninterference analysis starting...\n";
    Printf.printf "Output file: %s\n" output_filename;
    Printf.printf "AST has %d definitions\n" (List.length ast.defs);
    Printf.printf "\nAST definitions:\n";
    flush_all ()
  );
  
  check_ast env ast empty_ni_env

let _ =
  Target.register
    ~name:"noninterference"
    ~options:noninterference_options
    ~rewrites:[]
    noninterference_target