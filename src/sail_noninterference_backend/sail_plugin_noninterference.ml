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


let rec first_output (list : lattice list) : lattice list = 
    match list with 
       | [] -> []
       | first_el::rest_of_list -> first_el :: []

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
let string_of_lattice = function (*Simple conversion function used for debugging*)
  | Secret -> "Secret"
  | Public -> "Public"

let convert_string_to_lattice (var_name : string) : lattice =
  if String.starts_with ~prefix:"public_" var_name then (
        Public
      )
      else if String.starts_with ~prefix:"secret_" var_name then (
        Secret
      )
      else
        failwith ("Variable " ^ var_name ^ " does not have a valid prefix (public_ or secret_)")
let check_variable_lattice (ni_env : ni_env) (id: id) : ni_env = (*This is the function to check the lattice of a var based on its name. We update and return env*)
  let var_name = string_of_id id in
  (*Printf.printf "Checking variable %s in non-interference environment\n" var_name;*)
  match find_opt (string_of_id id) ni_env with (*We start by matching on the variable's lattice*)
  | Some Secret ->
      if String.starts_with ~prefix:"public_" var_name then (*In this case we check if the variable is being downgraded which we don't allow*)
        failwith ("Cannot downgrade secret variable " ^ var_name ^ " to public")
      else
        ni_env
  | Some Public ->
      if String.starts_with ~prefix:"secret_" var_name then (*In this case we check if the variable is being upgraded which we do allow*)
        add (string_of_id id) Secret ni_env
      else
        ni_env
  | None -> (*The two previous cases should return ni_env all the time since the name changing would mean the id changes but we keep it for completeness*)
      if String.starts_with ~prefix:"public_" var_name then (
        Printf.printf "Adding variable %s to non-interference environment as Public\n" var_name;
        add (string_of_id id) Public ni_env)
      else if String.starts_with ~prefix:"secret_" var_name then (
        Printf.printf "Adding variable %s to non-interference environment as Secret\n" var_name;
        add (string_of_id id) Secret ni_env
      )
      else
        failwith ("Variable " ^ var_name ^ " does not have a valid prefix (public_ or secret_)")

let is_binop id = (*Helper function to use in identifying a binary operation*)
  let op = string_of_id id in
  Printf.printf "Checking if operator %s is a binary operator\n" op;
  match op with
  | "+" | "-" | "*" | "/" | "&&" | "||" | "==" | "!=" | "<" | ">" | "<=" | ">=" -> true
  | _ -> false

  (*Infer_lattice is linked to check_expr. It is to be used when inferring lattices. That is we expect the lattices we infer to already be in the environment.
  This is really only relevant cases that can involve variables. As of such literals are handled as an edge case*)
let rec infer_lattice (ni_env : ni_env) (expr : 'a exp) : lattice = 
  match expr with
  | E_aux (E_lit _, _) -> Public (*Literal case*)
  | E_aux (E_app (_, args), _) -> (*BinOp case; The operator doesn't affect the non-interference properties*)
    if List.exists (fun e -> infer_lattice ni_env e = Secret) args then Secret else Public (* We run through the args and infer lattices. If one is secret, the entirety is treated as being secret*)
  | E_aux (E_id id, _) -> ( (*Variable case*)
    (*Printf.printf "Inferring lattice for variable %s\n" (string_of_id id);*)
      match find_opt (string_of_id id) ni_env with
      | Some lattice -> lattice
      | None -> failwith ("Cannot infer lattice for unknown variable " ^ string_of_id id)
    )
  | E_aux (E_assign (lexp, value), _) -> (*Assign case*)
    failwith "Not implemented yet assign"
  | E_aux (E_let (pat, exp, body), _) -> (*Let decl case*)
    (match pat with
    | P_aux (P_id id, _) ->
      (*we add the variable to the non-interference environment*)
        let ni_env' = check_variable_lattice ni_env id in
        let _ = infer_lattice ni_env' exp in
        infer_lattice ni_env' body
    | P_aux (P_typ (_, P_aux (P_id id, _)), _) ->
        (*we add the variable to the non-interference environment*)
        let ni_env' = check_variable_lattice ni_env id in
        let _ = infer_lattice ni_env' exp in
        infer_lattice ni_env' body
    | _ -> failwith "Unsupported pattern in let expression for lattice inference")
  | _ -> failwith "not supported in inference"

(*This function exists to check the security level when assigning a variable such that assignments in loop contexts work
    with non-interference*)  
let rec check_assignement (ni_env : ni_env) (lhs_lattice : lattice) (rhs_lattice : lattice) (id : string) : unit = 
  match check_security_level ni_env with
          | Secret -> (*If we are in a secret context we allow no assignements to public variables, but we allow all other*)
              (match (lhs_lattice, rhs_lattice) with
              | (Public, _) ->
                  failwith ("Non-interference violation: assigning value to public variable " ^ id ^ " in secret context")
              | _ -> () )
          | Public ->
          (match (lhs_lattice, rhs_lattice) with
          | (Public, Secret) ->
              failwith ("Non-interference violation: assigning secret value to public variable " ^ id ^ "in public context")
          | _ -> () )



  (*The main function check_expr serves to run through the ast tree and update our environment accordingly as well as check for non-interference violations*)
let rec check_expr (env : Type_check.env) (expr : 'a exp) (ni_env : ni_env) : unit = 
  (*Printf.printf "Checking expression for non-interference: %s\n" (string_of_exp expr);*)
  match expr with
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
      (*Need implementation*)
  | E_aux (E_app (id, args), _) -> (*function application*)
      Printf.printf "Checking function application for non-interference function %s\n" (string_of_id id);
      let function_lattices = find_function (string_of_id id) ni_env in
      let input_lattices, output_lattices = function_lattices in
      let args_lattices = List.map (infer_lattice ni_env) args in
      if List.length input_lattices <> List.length args_lattices then
        failwith ("Function " ^ string_of_id id ^ " called with incorrect number of arguments")
      else
        let _ = List.fold_left (fun acc (input_lat, arg_lat) ->
          check_assignement ni_env input_lat arg_lat (string_of_id id);
          acc
        ) () (List.combine input_lattices args_lattices) in
      ();

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
          let ni_env' = check_variable_lattice ni_env id in (*Update ni_env*)
          let lhs_lattice = find (string_of_id id) ni_env' in (*find the lattice for the left-hand side*)
          let rhs_lattice = infer_lattice ni_env value in (*infer the lattice for the right-hand side*)
          check_assignement ni_env lhs_lattice rhs_lattice (string_of_id id);
          check_expr env value ni_env'
      | LE_aux (LE_deref exp, _) -> check_expr env exp ni_env
      | _ -> failwith "Unsupported lexp in assignment")

  | E_aux (E_let (pat, exp, body), _) -> (*Let declarations*)
    (match pat with
    | P_aux (P_id id, _) ->
        let ni_env' = check_variable_lattice ni_env id in
        Printf.printf "Checking env for newly added variable %s: %s\n" (string_of_id id) (string_of_lattice (find (string_of_id id) ni_env'));
        let lhs_lattice = find (string_of_id id) ni_env' in
        check_expr env exp ni_env';
        let rhs_lattice = infer_lattice ni_env' exp in
        check_assignement ni_env lhs_lattice rhs_lattice (string_of_id id);
        check_expr env body ni_env' 
    | P_aux (P_typ (_, pat), _) -> (*Let decl with typed annotation*)
      (match pat with
      | P_aux (P_id id, _) ->
          let ni_env' = check_variable_lattice ni_env id in
          Printf.printf "Checking env for newly added variable %s: %s\n" (string_of_id id) (string_of_lattice (find (string_of_id id) ni_env'));
          let lhs_lattice = find (string_of_id id) ni_env' in
          check_expr env exp ni_env';
          let rhs_lattice = infer_lattice ni_env' exp in
          check_assignement ni_env lhs_lattice rhs_lattice (string_of_id id);
          check_expr env body ni_env' (*The body is the next expression*)
      | _ -> failwith "Unsupported pattern in typed let expression")
    | _ -> failwith "Unsupported pattern in let expression match")

  | E_aux (E_block exprs, _) -> (*Block expression*)
    List.iter (fun e -> check_expr env e ni_env) exprs

  | E_aux (E_id id, _) -> (*Var expression*)
    let ni_env' = check_variable_lattice ni_env id in (*Probably shouldn't have this since a var has to be declared before where 
    we probably added it to our environment. Thus this is kind of redundant.*)
    ()
  | E_aux (E_if (cond, then_exp, else_exp), _) -> (*If statement*)
  (*In an if statement we check the condition first. If the condition has anything to do with a 
    secret variable, we have to make sure that there are no assigments to public variables in 
  branches.*)
      check_expr env cond ni_env;
      (match infer_lattice ni_env cond with
      | Secret -> 
          Printf.printf "Condition is secret, checking branches with elevated security level\n";
          let ni_env' = set_security_level ni_env Secret in 
          check_expr env then_exp ni_env';
          check_expr env else_exp ni_env';
      | Public -> 
          Printf.printf "Condition is public, checking branches with public security level\n";
          check_expr env then_exp ni_env;
          check_expr env else_exp ni_env;)
          
  | E_aux (E_loop (_,  _, cond, body), _) -> (*Loop expression*)
        check_expr env cond ni_env;
        (match infer_lattice ni_env cond with
        | Secret -> 
            Printf.printf "Loop condition is secret, checking body with elevated security level\n";
            let ni_env' = set_security_level ni_env Secret in
            check_expr env body ni_env';
        | Public -> 
            Printf.printf "Loop condition is public, checking body with public security level\n";
            check_expr env body ni_env; )
  | E_aux (E_return e, _) -> (*Return stmt*)
    Printf.printf "Reached Return expression \n";
    check_expr env e ni_env
  | _ -> 
    Printf.printf "Expression type not supported in non-interference analysis: %s\n" (string_of_exp expr);
    ()  
  


let rec inputs_to_list (pat : 'a pat) (ni_env : ni_env) : lattice list =
   match pat with
    | P_aux (P_id id, _) -> convert_string_to_lattice (string_of_id id) :: []
    | P_aux (P_typ (_, pat), _) -> inputs_to_list pat ni_env
    | P_aux (P_tuple pats, _) -> List.fold_left (fun acc p -> inputs_to_list p ni_env @ acc) [] pats
    | _ -> Printf.printf("Unsupported input parameter for function inn add input to env: %s") (string_of_pat pat);
            []

let rec add_input_to_env (pat : 'a pat) (ni_env : ni_env) : ni_env =
   match pat with
    | P_aux (P_id id, _) -> Ni_env.add (string_of_id id) (convert_string_to_lattice (string_of_id id)) ni_env
    | P_aux (P_typ (_, pat), _) -> add_input_to_env pat ni_env
    | P_aux (P_tuple pats, _) -> List.fold_left (fun acc p -> add_input_to_env p acc) ni_env pats
    | _ -> Printf.printf("Unsupported input parameter for function inn add input to env: %s") (string_of_pat pat);
            ni_env

let rec outputs_to_list (expr : 'a exp) (ni_env : ni_env) : lattice list =
  let step recurse (acc : lattice list) ((E_aux (e_aux, _) as e) : 'a exp) : lattice list * 'a exp =
    match e_aux with
    | E_return ret_exp ->
        (match ret_exp with
        | E_aux (E_lit _, _) -> Public :: acc, e (*Literal case*)
        | E_aux (E_id id, _) -> ( (*Variable case*)
          (*Printf.printf "Inferring lattice for variable %s\n" (string_of_id id);*)
            match find_opt (string_of_id id) ni_env with
            | Some lattice -> lattice :: acc, e
            | None -> failwith ("Cannot infer lattice for unknown variable in outputs to list" ^ string_of_id id)
          )
          | _ -> failwith "Unsupported pattern in let expression for outputs to list")
    | _ ->
        recurse acc e
  in
  let returns, _ = Rewriter.foldin_exp step [] expr in
  let correct_order = List.rev returns in
  first_output correct_order (*We take the first return statement as the output lattice. This is a simplification that we make for now, but it should be sufficient for our current purposes. In the future, we might want to consider all return statements and check for consistency among them.*)

let add_functions_to_env (ast : Type_check.typed_ast) (ni_env : ni_env) : ni_env =
    List.fold_left (fun acc def ->
      match def with
      | DEF_aux (DEF_fundef (FD_aux (FD_function (_, _, funcls), _)), _) -> 
        List.fold_left (fun env (FCL_aux (FCL_funcl (id, pexp), _)) ->
          if string_of_id id = "main" || string_of_id id = "foo" then (
            Printf.printf "Adding function %s to non-inteference environment\n" (string_of_id id);
            match pexp with
            |Pat_aux (Pat_exp (input, body), _) ->
              let input_lattice_list = inputs_to_list input ni_env in
              let output_lattice_list = outputs_to_list body ni_env in
              let updated_env = add_function (string_of_id id) input_lattice_list output_lattice_list env in
              updated_env
            | _ -> env
          )
          else env
          ) acc funcls
      | _ -> acc
            ) ni_env ast.defs


let check_ast (env : Type_check.env) (ast : Type_check.typed_ast) (ni_env : ni_env) = 
  List.iter (fun def -> 
    match def with
    | DEF_aux (DEF_fundef (FD_aux (FD_function (_, _, funcls), _)), _) ->
        List.iter 
          (fun (FCL_aux (FCL_funcl (id, pexp), _)) ->
            if string_of_id id = "main" || string_of_id id = "foo" then (
              Printf.printf "Checking function %s for non-interference\n" (string_of_id id);
              match pexp with
              |Pat_aux (Pat_exp (input, body), _) -> 
                let added_input_env = add_input_to_env input ni_env in
                check_expr env body added_input_env
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
  
  let ni_env = add_functions_to_env ast empty_ni_env in
  check_ast env ast ni_env

let _ =
  Target.register
    ~name:"noninterference"
    ~options:noninterference_options
    ~rewrites:[]
    noninterference_target