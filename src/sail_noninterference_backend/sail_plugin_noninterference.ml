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
let string_of_lattice = function
  | Secret -> "Secret"
  | Public -> "Public"


let check_variable_lattice (ni_env : ni_env) (id: id) : ni_env =
  let var_name = string_of_id id in
  Printf.printf "Checking variable %s in non-interference environment\n" var_name;
  match find_opt (string_of_id id) ni_env with
  | Some Secret ->
      if String.starts_with ~prefix:"public_" var_name then
        failwith ("Cannot downgrade secret variable " ^ var_name ^ " to public")
      else
        ni_env
  | Some Public ->
      if String.starts_with ~prefix:"secret_" var_name then
        add (string_of_id id) Secret ni_env
      else
        ni_env
  | None ->
      if String.starts_with ~prefix:"public_" var_name then (
        Printf.printf "Adding variable %s to non-interference environment as Public\n" var_name;
        add (string_of_id id) Public ni_env)
      else if String.starts_with ~prefix:"secret_" var_name then (
        Printf.printf "Adding variable %s to non-interference environment as Secret\n" var_name;
        add (string_of_id id) Secret ni_env
        
      )
        
      else
        failwith ("Variable " ^ var_name ^ " does not have a valid prefix (public_ or secret_)")

let is_binop id =
  let op = string_of_id id in
  match op with
  | "+" | "-" | "*" | "/" | "&&" | "||" | "==" | "!=" | "<" | ">" | "<=" | ">=" -> true
  | _ -> false

let rec infer_lattice (ni_env : ni_env) (expr : 'a exp) : lattice =
  match expr with
  | E_aux (E_id id, _) -> (
    Printf.printf "Inferring lattice for variable %s\n" (string_of_id id);
      find (string_of_id id) ni_env
    )
  | E_aux (E_lit _, _) -> Public
  | E_aux (E_app (_, args), _) ->
      if List.exists (fun e -> infer_lattice ni_env e = Secret) args then Secret else Public
  | E_aux (E_let (P_aux (P_typ (_, pat), _), exp, body), _) ->
    (* Handle typed pattern *)
    (match pat with
     | P_aux (P_id id, _) ->
         let ni_env' = check_variable_lattice ni_env id in
         infer_lattice ni_env' body
     | _ -> Public)
  | _ -> failwith "not supported"

let rec check_expr (env : Type_check.env) (expr : 'a exp) (ni_env : ni_env) : unit = 
  Printf.printf "Checking expression for non-interference: %s\n" (string_of_exp expr);

  match expr with
  | E_aux (E_app (id, [e1; e2]), _) when is_binop id ->
      check_expr env e1 ni_env;
      check_expr env e2 ni_env;
      Printf.printf "Checking binop expression for non-interference\n";
      (*
      Any operator is treated the same in non-interference analysis since it does not matter
      what the operator is. If we have an expression like "x + y", the operator doesn't affect
      the non-interference properties since what we are really interested in, is whether x or y
      are secret and whether they are assigned to a public or secret variable.
      *)
      
      
      
  | E_aux (E_lit lit, _) ->
    (match lit with
    | L_aux (L_unit, _) -> ()
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
  | E_aux (E_assign (lexp, value), _) -> (*Assign expressions*)
    (*
    For assignments, we have different cases to consider for the left hand side. Thus we refer to a 
    helper function check_lexp to handle the different cases. These will matter for our non-interference
    analysis. We allow an assignment to go through in three out of four cases.
    - If lexp and value are both public, then the assignment is fine.
    - If lexp is secret and value is public, then the assignment is fine.
    - If lexp is secret and value is secret, then the assignment is fine.
    - If lexp is public and value is secret, then this is a violation of non-interference and should result in a skip.
    *)
      (match lexp with 
      | LE_aux (LE_id id, _) ->
          let ni_env' = check_variable_lattice ni_env id in
          let lhs_lattice = find (string_of_id id) ni_env' in
          let rhs_lattice = infer_lattice ni_env value in
          (match (lhs_lattice, rhs_lattice) with
          | (Public, Secret) ->
              failwith ("Non-interference violation: assigning secret value to public variable " ^ string_of_id id)
          | _ ->
              check_expr env value ni_env')
      | LE_aux (LE_deref exp, _) -> check_expr env exp ni_env
      | _ -> failwith "Unsupported lexp in assignment")
  | E_aux (E_let (pat, exp, body), _) -> (*Let declarations*)
    Printf.printf "Checking let expression for non-interference\n";
    (match pat with
    | P_aux (P_id id, _) ->
      check_expr env exp ni_env;
      let ni_env' = check_variable_lattice ni_env id in
      check_expr env body ni_env';
      Printf.printf "Checking env for newly added variable %s: %s\n" (string_of_id id) (string_of_lattice (find (string_of_id id) ni_env'));
      let lhs_lattice = find (string_of_id id) ni_env' in
      let rhs_lattice = infer_lattice ni_env exp in
      Printf.printf "1";
      (match (lhs_lattice, rhs_lattice) with
      | (Public, Secret) ->
          failwith ("Non-interference violation: assigning secret value to public variable " ^ string_of_id id)
      | _ ->
      check_expr env body ni_env')
    | P_aux (P_typ (_, pat), _) ->
      Printf.printf "Checking let expression with type annotation for non-interference\n";
      (match pat with
      | P_aux (P_id id, _) ->
          let ni_env' = check_variable_lattice ni_env id in
          Printf.printf "Checking env for newly added variable %s: %s\n" (string_of_id id) (string_of_lattice (find (string_of_id id) ni_env'));
          let lhs_lattice = find (string_of_id id) ni_env' in
          Printf.printf "found lhs lattice \n";
          let rhs_lattice = infer_lattice ni_env' exp in
          Printf.printf "2";
          (match (lhs_lattice, rhs_lattice) with
          | (Public, Secret) ->
              failwith ("Non-interference violation: assigning secret value to public variable " ^ string_of_id id)
          | _ ->
          check_expr env body ni_env') (*The body is the next expression*)
      | _ -> failwith "Unsupported pattern in let expression2")
    | _ -> failwith "Unsupported pattern in let expression")
  | E_aux (E_block exprs, _) -> (*Block expression*)
    Printf.printf "Checking block expression for non-interference\n";
    List.iter (fun e -> check_expr env e ni_env) exprs
  | E_aux (E_id id, _) -> (*Var expression*)
    Printf.printf "Checking variable expression for non-interference: %s\n" (string_of_id id);
    let ni_env' = check_variable_lattice ni_env id in
    ()
  | E_aux (E_if (cond, then_exp, else_exp), _) ->
      check_expr env cond ni_env;
      check_expr env then_exp ni_env;
      check_expr env else_exp ni_env;
      (* handle if statement *)
  | E_aux (E_return e, _) -> (*Return stmt*)
    Printf.printf "Checking return expression for non-interference\n";
    check_expr env e ni_env
  | _ -> ()  
  

let check_ast (env : Type_check.env) (ast : Type_check.typed_ast) (ni_env : ni_env) = 
  List.iter (fun def -> 
    match def with
    | DEF_aux (DEF_fundef (FD_aux (FD_function (_, _, funcls), _)), _) ->
        List.iter 
          (fun (FCL_aux (FCL_funcl (id, pexp), _)) ->
            if string_of_id id = "main" then (
              Printf.printf "Checking function %s for non-interference\n" (string_of_id id);
              match pexp with
              |Pat_aux (Pat_exp (_, body), _) -> check_expr env body ni_env
              | _ -> ()))
          funcls 
    | _ -> ()
  ) ast.defs  
let defs_without_includes defs =
    let rec go depth acc = function
      | DEF_aux (DEF_pragma ("include_start", _), _) :: rest -> go (depth + 1) acc rest
      | DEF_aux (DEF_pragma ("include_end", _), _) :: rest -> go (max 0 (depth - 1)) acc rest
      | def :: rest when depth = 0 -> go depth (def :: acc) rest
      | _ :: rest -> go depth acc rest
      | [] -> List.rev acc
    in
    go 0 [] defs

let noninterference_target out_file { ast; effect_info; env; _ } =
  let output_filename = match out_file with 
    | Some f -> f ^ ".noninterference" 
    | None -> "noninterference_analysis" 
  in
  let open Ast in
  let open Ast_defs in
  

  if !opt_verbose then (
    Printf.printf "Noninterference analysis starting...\n";
    Printf.printf "Output file: %s\n" output_filename;
    Printf.printf "AST has %d definitions\n" (List.length ast.defs);
    Printf.printf "\nAST definitions:\n";
    let filename = Option.value ~default:"outnoninterference.sail" None in
    let chan = open_out filename in
    let stripped = Type_check.strip_ast ast in
    let local_only_ast = { stripped with defs = defs_without_includes stripped.defs } in
    Pretty_print_sail.output_ast chan local_only_ast;
    close_out chan;
    flush_all ()
  );
  
  check_ast env ast empty_ni_env

let _ =
  Target.register
    ~name:"noninterference"
    ~options:noninterference_options
    ~rewrites:[]
    noninterference_target