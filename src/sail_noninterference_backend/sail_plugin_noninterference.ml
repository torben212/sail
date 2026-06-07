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


let first_lattice (list : lattice list) : lattice =
  match list with
  | [] -> failwith "Empty list"
  | first_el::_ -> first_el

(* Options for noninterference plugin *)
let opt_output_dir = ref (Some ".")
let opt_verbose = ref false

let opt_security_level = ref None
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
    ( Flag.create ~prefix:["noninterference"] ~arg:"security_level" "security_level",
      Arg.String (fun lat ->
        match lat with
        | "User" -> opt_security_level := Some User
        | "Supervisor" -> opt_security_level := Some Supervisor
        | "Machine" -> opt_security_level := Some Machine
        | _ -> failwith "Invalid security level. Valid options are: User, Supervisor, Machine"
      ),
      "set the initial security level for noninterference analysis (User, Supervisor, Machine)"
    );
  ]
let contains_substring s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec aux i =
    if i > len_s - len_sub then false
    else if String.sub s i len_sub = sub then true
    else aux (i + 1)
  in
  aux 0
let string_of_lattice = function (*Simple conversion function used for debugging*)
  | Secret -> "Secret"
  | Public -> "Public"
  | User -> "User"
  | Supervisor -> "Supervisor"
  | Machine -> "Machine"


let string_of_mutability = function (*Simple conversion function used for debugging*)
  | Mutable -> "Mutable"
  | Fragile -> "Fragile"

let convert_string_to_lattice (var_name : string) : lattice =
  if contains_substring var_name "public_" then (
        Public
      )
      else if contains_substring var_name "secret_" then (
        Secret
      )
      else if contains_substring var_name "user_" then (
        User
      )
      else if contains_substring var_name "supervisor_" then (
        Supervisor
      )
      else if contains_substring var_name "machine_" then (
        Machine
      )
      else
        failwith ("Variable " ^ var_name ^ " does not have a valid prefix (public_ or secret_)")

let check_mutability (ni_env : ni_env) (id : string) : ni_env =
  if contains_substring id "fragile_" then (
    Printf.printf "Adding variable %s to non-interference environment as Fragile\n" id;
    add_mutability id Fragile ni_env)
  else if contains_substring id "mutable_" then (
    Printf.printf "Adding variable %s to non-interference environment as Mutable\n" id;
    add_mutability id Mutable ni_env)
  else
    ni_env (*If there is no mutability prefix, we just don't add it to the mutability environment and treat it as a normal variable*)
    
let check_variable_lattice (ni_env : ni_env) (id: string) : ni_env = (*This is the function to check the lattice of a var based on its name. We update and return env*)
  (*Printf.printf "Checking variable %s in non-interference environment\n" var_name;*)
  match find_opt id ni_env with (*We start by matching on the variable's lattice*)
  | Some Secret ->
      if contains_substring id "public_" then (  (*In this case we check if the variable is being downgraded which we don't allow*)
        failwith ("Cannot downgrade secret variable " ^ id ^ " to public"))
      else 
        ni_env
  | Some Public ->
      if contains_substring id "secret_" then ( (*In this case we check if the variable is being upgraded which we do allow*)
        add id Secret ni_env
      )
      else
        ni_env
        (*New lattices*)
  | Some User ->
      if contains_substring id "supervisor_" then (
        add id Supervisor ni_env
      )
      else if contains_substring id "machine_" then (
        add id Machine ni_env
      )
      else ni_env
  | Some Supervisor ->
      if contains_substring id "machine_" then (
        add id Machine ni_env
      )
      else ni_env
  | Some Machine -> ni_env
  | None -> (*The two previous cases should return ni_env all the time since the name changing would mean the id changes but we keep it for completeness*)
      if contains_substring id "public_" then ( (*In this case we check if the variable is being added as public*)
        Printf.printf "Adding variable %s to non-interference environment as Public\n" id;
        let ni_env' = add id Public ni_env in
        let ni_env'' = check_mutability ni_env' id in
        ni_env''
      ) 
      else if contains_substring id "secret_" then ( (*In this case we check if the variable is being added as secret*)
        Printf.printf "Adding variable %s to non-interference environment as Secret\n" id;
        let ni_env' = add id Secret ni_env in
        let ni_env'' = check_mutability ni_env' id in
        ni_env'')
      (*New lattices*)
      else if contains_substring id "user_" then (
        Printf.printf "Adding variable %s to non-interference environment as User\n" id;
        let ni_env' = add id User ni_env in
        let ni_env'' = check_mutability ni_env' id in
        ni_env''
      )
      else if contains_substring id "supervisor_" then (
        Printf.printf "Adding variable %s to non-interference environment as Supervisor\n" id;
        let ni_env' = add id Supervisor ni_env in
        let ni_env'' = check_mutability ni_env' id in
        ni_env''
      )
      else if contains_substring id "machine_" then (
        Printf.printf "Adding variable %s to non-interference environment as Machine\n" id;
        let ni_env' = add id Machine ni_env in
        let ni_env'' = check_mutability ni_env' id in
        ni_env''
      )
      else failwith ("Variable " ^ id ^ " does not have a valid security lattice (public_ or secret_)")


let is_binop id = (*Helper function to use in identifying a binary operation*)
  let op = string_of_id id in
  Printf.printf "Checking if operator %s is a binary operator\n" op;
    match op with
    | "+" | "-" | "*" | "/" | "&&" | "||" | "|" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "add_atom"
    | "sub_atom" | "mult_atom" | "gt_int" | "lt_int" | "lteq_int" | "gteq_int" | "eq_int" | "neq_int"
    | "eq_bool" | "neq_bool" | "or_bool" | "add_bits" | "eq_string" -> true
    | _ -> false

    (*This function exists to check the security level when assigning a variable such that assignments in loop contexts work
    with non-interference*)  
let rec check_assignment (ni_env : ni_env) (lhs_lattice : lattice) (rhs_lattice : lattice) (id : string) : unit = 
    (match check_security_level ni_env with
      | Secret -> (*If we are in a secret context we allow no assignements to public variables, but we allow all other*)
        (match (lhs_lattice, rhs_lattice) with
        | (Public, _) ->
          failwith ("Non-interference violation: assigning value to public variable " ^ id ^ " in secret context")
        | _ -> () )
      | Public ->
        (match (lhs_lattice, rhs_lattice) with
        | (Public, Secret) ->
          failwith ("Non-interference violation: assigning secret value to public variable " ^ id ^ "in public context")
        | (User, Supervisor) -> (*We need to add checks for user, supervisor and machine here since default is Public context*)
          if (get_ass_sec_lev ni_env = User) then
            failwith ("Non-interference violation: assigning supervisor value to user variable " ^ id ^ " in public context with level " ^ string_of_lattice (get_ass_sec_lev ni_env))
        | (User, Machine) ->
          if (get_ass_sec_lev ni_env != Machine) then
            failwith ("Non-interference violation: assigning machine value to user variable " ^ id ^ " in public context with level " ^ string_of_lattice (get_ass_sec_lev ni_env))
        | (Supervisor, Machine) -> 
          if (get_ass_sec_lev ni_env != Machine) then
            failwith ("Non-interference violation: assigning machine value to supervisor variable " ^ id ^ " in public context with level " ^ string_of_lattice (get_ass_sec_lev ni_env))
        | _ -> () )
      | User ->
        (match (lhs_lattice, rhs_lattice) with
        | (User, Supervisor) -> 
          if (get_ass_sec_lev ni_env = User) then
            failwith ("Non-interference violation: assigning supervisor value to user variable " ^ id ^ " in public context with level " ^ string_of_lattice (get_ass_sec_lev ni_env))
        | (User, Machine) ->
          if (get_ass_sec_lev ni_env != Machine) then
            failwith ("Non-interference violation: assigning machine value to user variable " ^ id ^ " in public context with level " ^ string_of_lattice (get_ass_sec_lev ni_env))
        | (Supervisor, Machine) -> 
          if (get_ass_sec_lev ni_env != Machine) then
            failwith ("Non-interference violation: assigning machine value to supervisor variable " ^ id ^ " in public context with level " ^ string_of_lattice (get_ass_sec_lev ni_env))
        | _ -> () )
      | Supervisor ->
        (match (lhs_lattice, rhs_lattice) with
        | (User, Supervisor) ->
          if (get_ass_sec_lev ni_env = User) then
            failwith ("Non-interference violation: assigning value to user variable " ^ id ^ " in supervisor context with level " ^ string_of_lattice (get_ass_sec_lev ni_env))
        | (User, Machine) ->
          if (get_ass_sec_lev ni_env != Machine) then
            failwith ("Non-interference violation: assigning machine value to user variable " ^ id ^ " in supervisor context with level " ^ string_of_lattice (get_ass_sec_lev ni_env))
        | (Supervisor, Machine) ->
          if (get_ass_sec_lev ni_env != Machine) then
          failwith ("Non-interference violation: assigning machine value to supervisor variable " ^ id ^ " in supervisor context with level " ^ string_of_lattice (get_ass_sec_lev ni_env))
        | _ -> () )
      | Machine ->
        (match (lhs_lattice, rhs_lattice) with
        | (User, _) ->
          if (get_ass_sec_lev ni_env != Machine) then
            failwith ("Non-interference violation: assigning value to user variable " ^ id ^ " in machine context with level " ^ string_of_lattice (get_ass_sec_lev ni_env))
        | (Supervisor, _) ->
          if (get_ass_sec_lev ni_env != Machine) then
            failwith ("Non-interference violation: assigning supervisor value to supervisor variable " ^ id ^ " in machine context with level " ^ string_of_lattice (get_ass_sec_lev ni_env))
        | _ -> () ))


  (*Infer_lattice is linked to check_expr. It is to be used when inferring lat tices. That is we expect the lattices we infer to already be in the environment.
  This is really only relevant cases that can involve variables. As of such literals are handled as an edge case*)
let rec infer_lattice (ni_env : ni_env) (expr : 'a exp) : lattice list = 
  match expr with
  | E_aux (E_lit _, _) -> 
    if (get_ass_sec_lev ni_env = User || get_ass_sec_lev ni_env = Supervisor || get_ass_sec_lev ni_env = Machine) then [User] (*These cases are for when we use multiple lattices to simulate Risc-V priv levels*)
    else
    [Public] (*Literal case*)
  | E_aux (E_app (id, args), _) when is_binop id -> (*BinOp case; The operator doesn't affect the non-interference properties*)
  if List.exists (fun e -> infer_lattice ni_env e = [Machine]) args then [Machine] else if List.exists (fun e -> infer_lattice ni_env e = [Supervisor]) args then [Supervisor] else if List.exists (fun e -> infer_lattice ni_env e = [User]) args then [User]

  else (
    if List.exists (fun e -> infer_lattice ni_env e = [Secret]) args then [Secret] else [Public] (* We run through the args and infer lattices. If one is secret, the entirety is treated as being secret*)
  )
  | E_aux (E_app (id, args), _) -> ( (*Function call case *)
      Printf.printf "Inferring lattice for function application of function %s\n" (string_of_id id);
      let function_lattices = find_function (string_of_id id) ni_env in
      let input_lattices, output_lattices = function_lattices in
      output_lattices  
  )
  | E_aux (E_id id, _) -> ( (*Variable case*)
    (*Printf.printf "Inferring lattice for variable %s\n" (string_of_id id);*)
      match find_opt (string_of_id id) ni_env with
      | Some lattice -> [lattice]
      | None -> failwith ("Cannot infer lattice for unknown variable " ^ string_of_id id)
    )
  | E_aux (E_let (pat, exp, body), _) -> (*Let decl case*)
    (match pat with
    | P_aux (P_id id, _) ->
      (*we add the variable to the non-interference environment*)
        let ni_env' = check_variable_lattice ni_env (string_of_id id) in
        let _ = infer_lattice ni_env' exp in
        infer_lattice ni_env' body
    | P_aux (P_typ (_, P_aux (P_id id, _)), _) ->
        (*we add the variable to the non-interference environment*)
        let ni_env' = check_variable_lattice ni_env (string_of_id id) in
        let _ = infer_lattice ni_env' exp in
        infer_lattice ni_env' body
    | _ -> failwith "Unsupported pattern in let expression for lattice inference")
  | _ -> failwith "not supported in inference"


(*The main function check_expr serves to run through the ast tree and update our environment accordingly as well as check for non-interference violations*)
let rec check_expr (env : Type_check.env) (expr : 'a exp) (ni_env : ni_env) : ni_env = 
  (*Printf.printf "Checking expression for non-interference: %s\n" (string_of_exp expr);*)
  match expr with
  | E_aux (E_lit lit, _) ->
    (match lit with
    | L_aux (L_unit, _) -> ni_env
    | L_aux (L_num _, _) -> ni_env
    | L_aux (L_true, _) | L_aux (L_false, _) -> ni_env
    | L_aux (L_real _, _) -> ni_env
    | L_aux (L_string _, _) -> ni_env
    | L_aux (L_hex _, _) -> ni_env
    | _ -> failwith "Unsupported literal type")
    (* 
    We match literals here to limit what types of literals we currently support in our non-interference
    analysis. This is more of a safety measure to ensure our analysis doesn't fail because of an 
    unsupported literal type.
    *)
    
  | E_aux (E_app (id, [e1; e2]), _) when is_binop id ->
      let ni_env' = check_expr env e1 ni_env in
      let ni_env'' = check_expr env e2 ni_env' in
      Printf.printf "Checking binop expression for non-interference\n";
      ni_env''
      (*
      Any operator is treated the same in non-interference analysis since it does not matter
      what the operator is. If we have an expression like "x + y", the operator doesn't affect
      the non-interference properties since what we are really interested in, is whether x or y
      are secret and whether they are assigned to a public or secret variable.
      *)
  | E_aux (E_app (id, args), _) -> (*function call*)
  (*For function calls we need to check the lattices of the arguments and the return value. Right
  now we only compare the inferred arg lattices with the expected input lattices and treat them accordingly*)
      Printf.printf "Checking function application for non-interference function %s\n" (string_of_id id);
      let function_lattices = find_function (string_of_id id) ni_env in
      let input_lattices, output_lattices = function_lattices in
      let args_lattices = List.fold_left (fun acc arg ->
        let arg_lattice = infer_lattice ni_env arg in
        acc @ arg_lattice)
        [] args in
      if List.length input_lattices <> List.length args_lattices then
        failwith ("Function " ^ string_of_id id ^ " called with incorrect number of arguments")
      else
        let _ = List.fold_left (fun acc (input_lat, arg_lat) ->
          check_assignment ni_env input_lat arg_lat (string_of_id id);
          acc
        ) () (List.combine input_lattices args_lattices) in
      ni_env;

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
        Printf.printf "Checking assignment to variable %s for non-interference\n" (string_of_id id);
          let ni_env' = check_variable_lattice ni_env (string_of_id id) in (*Update ni_env*)
          (match find_mutability_opt (string_of_id id) ni_env' with
          | Some Fragile ->
            failwith ("Assignment variable " ^ string_of_id id ^ " is fragile and cannot be assigned to\n")
          | _ -> ();
          let lhs_lattice = find (string_of_id id) ni_env' in (*find the lattice for the left-hand side*)
          let rhs_lattice = infer_lattice ni_env' value in (*infer the lattice for the right-hand side*)
          check_assignment ni_env' lhs_lattice (first_lattice rhs_lattice) (string_of_id id);
          check_expr env value ni_env')
      | _ -> failwith "Unsupported lexp in assignment")

  | E_aux (E_let (pat, exps, body), _) -> (*Let declarations*)
    (match pat with
    | P_aux (P_id id, _) ->
        let ni_env' = check_variable_lattice ni_env (string_of_id id) in
        Printf.printf "Checking env for newly added variable %s: %s\n" (string_of_id id) (string_of_lattice (find (string_of_id id) ni_env'));
        let mutability = match find_mutability_opt (string_of_id id) ni_env' with
          | Some mut -> mut
          | None -> Mutable
        in
        Printf.printf "Checking env for newly added variable %s: %s\n" (string_of_id id) (string_of_mutability mutability);
        let lhs_lattice = find (string_of_id id) ni_env' in
        let ni_env'' = check_expr env exps ni_env' in
        let rhs_lattice = infer_lattice ni_env'' exps in
        check_assignment ni_env lhs_lattice (first_lattice rhs_lattice) (string_of_id id);
        check_expr env body ni_env'' 

    | P_aux (P_typ (_, pat), _) -> (*Let decl with typed annotation*)
      (match pat with
      | P_aux (P_id id, _) ->
          let ni_env' = check_variable_lattice ni_env (string_of_id id) in
          Printf.printf "Checking env for newly added variable %s: %s\n" (string_of_id id) (string_of_lattice (find (string_of_id id) ni_env'));
          let mutability = match find_mutability_opt (string_of_id id) ni_env' with
            | Some mut -> mut
            | None -> Mutable
          in
          Printf.printf "Checking env for newly added variable %s: %s\n" (string_of_id id) (string_of_mutability mutability);
          let lhs_lattice = find (string_of_id id) ni_env' in
          let ni_env'' = check_expr env exps ni_env' in
          let rhs_lattice = infer_lattice ni_env'' exps in
          check_assignment ni_env lhs_lattice (first_lattice rhs_lattice) (string_of_id id);
          check_expr env body ni_env'' (*The body is the next expression*)

      | _ -> failwith "Unsupported pattern in typed let expression")

    | P_aux (P_tuple pats, _) -> ( (*let decl med tuplle pattern*)
          let exp_lattice_list = infer_lattice ni_env exps in
          if List.length pats <> List.length exp_lattice_list then
            failwith ((Printf.sprintf "let assignement with tuple pattern has different number of patterns: %d and expressions: %d" (List.length pats) (List.length exp_lattice_list)))
          else
          let updat = List.fold_left (fun ni_env (pat, lattice) -> 
            Printf.printf "Checking tuple pattern %s \n" (string_of_pat pat);
            let updated_env = check_variable_lattice ni_env (string_of_pat pat) in
            let lhs_lattice = find (string_of_pat pat) updated_env in
            check_assignment ni_env lhs_lattice lattice (string_of_pat pat);
            updated_env
            ) ni_env (List.combine pats exp_lattice_list) in
            check_expr env body updat
      )
    | _ -> failwith "Unsupported pattern in let expression match")
  
  | E_aux (E_var (lexp, exps, body), _) ->  (*Var declaration*)
      (match lexp with
      | LE_aux (LE_id id, _) ->
          let ni_env' = check_variable_lattice ni_env (string_of_id id) in
          let ni_env'' = check_expr env exps ni_env' in
          let lhs_lattice = find (string_of_id id) ni_env'' in
          let rhs_lattice = infer_lattice ni_env'' exps in
          check_assignment ni_env lhs_lattice (first_lattice rhs_lattice) (string_of_id id);
          check_expr env body ni_env''
      | LE_aux (LE_typ (_, id), _) ->
          let ni_env' = check_variable_lattice ni_env (string_of_id id) in
          let ni_env'' = check_expr env exps ni_env' in
          let lhs_lattice = find (string_of_id id) ni_env'' in
          let rhs_lattice = infer_lattice ni_env'' exps in
          check_assignment ni_env lhs_lattice (first_lattice rhs_lattice) (string_of_id id);
          check_expr env body ni_env''
      | _ -> failwith "Unsupported lexp in var declaration")

  | E_aux (E_block exprs, _) -> (*Block expression*)
    List.fold_left (fun current_ni_env e -> check_expr env e current_ni_env) ni_env exprs

  | E_aux (E_id id, _) -> (*Var expression*)
    let ni_env' = check_variable_lattice ni_env (string_of_id id) in (*Probably shouldn't have this since a var has to be declared before where 
    we probably added it to our environment. Thus this is kind of redundant.*)
    ni_env'

  | E_aux (E_if (cond, then_exp, else_exp), _) -> (*If statement*)
  (*In an if statement we check the condition first. If the condition has anything to do with a 
    secret variable, we have to make sure that there are no assigments to public variables in 
  branches.*)
      let ni_env' = check_expr env cond ni_env in
      (match (first_lattice (infer_lattice ni_env' cond)) with
      | Secret -> 
          Printf.printf "Condition is secret, checking branches with elevated security level\n";
          let ni_env'' = set_security_level ni_env' Secret in 
          let _ = check_expr env then_exp ni_env'' in
          let _ = check_expr env else_exp ni_env'' in
          ni_env';
      | Public -> 
          Printf.printf "Condition is public, checking branches with public security level\n";
          let _ = check_expr env then_exp ni_env' in
          let _ = check_expr env else_exp ni_env' in
          ni_env';
      | User ->
          Printf.printf "Condition is user level, checking branches according to priv level\n";
          let ni_env'' = set_security_level ni_env' User in
          let _ = check_expr env then_exp ni_env'' in
          let _ = check_expr env else_exp ni_env'' in
          ni_env';
      | Supervisor -> 
          Printf.printf "Condition is supervisor level, checking branches according to priv level\n";
          let ni_env'' = set_security_level ni_env' Supervisor in
          let _ = check_expr env then_exp ni_env'' in
          let _ = check_expr env else_exp ni_env'' in
          ni_env';
      | Machine ->
          Printf.printf "Condition is Machine level, checking branches according to priv level\n";
          let ni_env'' = set_security_level ni_env' Machine in
          let _ = check_expr env then_exp ni_env'' in
          let _ = check_expr env else_exp ni_env'' in
          ni_env';)

  | E_aux (E_loop (_,  _, cond, body), _) -> (*Loop expression*)
        let ni_env' = check_expr env cond ni_env in
        (match (first_lattice (infer_lattice ni_env' cond)) with
        | Secret -> 
            Printf.printf "Loop condition is secret, checking body with elevated security level\n";
            let ni_env'' = set_security_level ni_env' Secret in
            let _ = check_expr env body ni_env'' in
            ni_env'
        | Public -> 
            Printf.printf "Loop condition is public, checking body with public security level\n";
            let _ = check_expr env body ni_env' in
            ni_env';
        | User ->
            Printf.printf "Loop condition is user level, checking body according to priv level\n";
            let ni_env'' = set_security_level ni_env' User in
            let _ = check_expr env body ni_env'' in
            ni_env'
        | Supervisor ->
            Printf.printf "Loop condition is supervisor level, checking body according to priv level\n";
            let ni_env'' = set_security_level ni_env' Supervisor in
            let _ = check_expr env body ni_env'' in
            ni_env'
        | Machine ->
            Printf.printf "Loop condition is machine level, checking body according to priv level\n";
            let ni_env'' = set_security_level ni_env' Machine in
            let _ = check_expr env body ni_env'' in
            ni_env'
        )
  | E_aux (E_return e, _) -> (*Return stmt*)
    Printf.printf "Reached Return expression \n";
    check_expr env e ni_env
  | _ -> 
    Printf.printf "Expression type not supported in non-interference analysis: %s\n" (string_of_exp expr);
    ni_env
  


let rec inputs_to_list (pat : 'a pat) (ni_env : ni_env) : lattice list =
   match pat with
    | P_aux (P_id id, _) -> convert_string_to_lattice (string_of_id id) :: []
    | P_aux (P_typ (_, pat), _) -> inputs_to_list pat ni_env
    | P_aux (P_tuple pats, _) -> List.fold_left (fun acc p -> acc @ inputs_to_list p ni_env) [] pats
    | P_aux (P_var (pat, _), _) -> inputs_to_list pat ni_env
    | P_aux (P_app (_, pats), _) -> List.fold_left (fun acc p -> acc @ inputs_to_list p ni_env) [] pats
    | P_aux (P_list pats, _) -> List.fold_left (fun acc p -> acc @ inputs_to_list p ni_env) [] pats
    | _ -> Printf.printf("Unsupported input parameter for function in inputs to list: %s\n") (string_of_pat pat);
            []

let rec add_input_to_env (pat : 'a pat) (ni_env : ni_env) : ni_env =
   let ni_env' = (match pat with 
    | P_aux (P_id id, _) -> check_variable_lattice ni_env (string_of_id id)
    | P_aux (P_typ (_, pat), _) -> add_input_to_env pat ni_env
    | P_aux (P_var (pat, _), _) -> add_input_to_env pat ni_env
    | P_aux (P_app (_, pats), _) -> List.fold_left (fun acc p -> add_input_to_env p acc) ni_env pats
    | P_aux (P_tuple pats, _) -> List.fold_left (fun acc p -> add_input_to_env p acc) ni_env pats
    | P_aux (P_list pats, _) -> List.fold_left (fun acc p -> add_input_to_env p acc) ni_env pats
    | _ -> 
      match string_of_pat pat with
      | "()" -> ni_env
      | _ -> failwith (Printf.sprintf "Unsupported input parameter for function in add input to env: %s" (string_of_pat pat))) in
  ni_env'

  
let rec outputs_to_list (expr : 'a exp) (ni_env : ni_env) : lattice list =
  let step recurse (acc : lattice list) ((E_aux (e_aux, _) as e) : 'a exp) : lattice list * 'a exp =
    match e_aux with
    | E_return ret_exp ->
        (match ret_exp with
        | E_aux (E_lit _, _) -> Public :: acc, e (*Literal case*)
        | E_aux (E_id id, _) -> ( (*Variable case*)
          let ni_env' = check_variable_lattice ni_env (string_of_id id) in
          let lattice = find (string_of_id id) ni_env' in 
          (match lattice with
          | lat -> lattice :: acc, e))
        | E_aux (E_tuple exps, _) -> (
          let lattices = List.fold_left (fun (lats) exp ->
            match exp with
            | E_aux (E_lit _, _) -> Public :: lats
            | E_aux (E_id id, _) -> 
              let ni_env' = check_variable_lattice ni_env (string_of_id id) in
              let lattice = find (string_of_id id) ni_env' in
              lattice :: lats
            | _ -> failwith "Unsupported pattern in tuple expression for outputs to"
          ) ([]) exps in
          lattices @ acc, e
            )
        | _ -> failwith (Printf.sprintf "Unsupported input parameter for function in add input to env: %s" (string_of_exp ret_exp)))
    | _ ->
        recurse acc e
  in
  let returns, _ = Rewriter.foldin_exp step [] expr in
  List.rev returns
  (*We take the first return statement as the output lattice. This is a simplification that we make for now, but it should be sufficient for our current purposes. In the future, we might want to consider all return statements and check for consistency among them.*)

let add_functions_to_env (env : Type_check.env) (ast : Type_check.typed_ast) (ni_env : ni_env) : ni_env =
    List.fold_left (fun acc def ->
      match def with
      | DEF_aux (DEF_fundef (FD_aux (FD_function (_, _, funcls), _)), _) -> 
        List.fold_left (fun env1 (FCL_aux (FCL_funcl (id, pexp), _)) ->
          Printf.printf "Adding function %s to non-inteference environment\n" (string_of_id id);
          match pexp with
          |Pat_aux (Pat_exp (input, body), _) ->
            (*Vi burde lave et match case her for at sikre os at der faktisk er inputs*)
            let input_lattice_list = inputs_to_list input ni_env in
            let output_lattice_list = outputs_to_list body ni_env in
            let updated_env = add_function (string_of_id id) input_lattice_list output_lattice_list env1 in
            updated_env
          | _ -> env1
          ) acc funcls
      | _ -> acc
            ) ni_env ast.defs


let check_ast (env : Type_check.env) (ast : Type_check.typed_ast) (ni_env : ni_env) = 
  (List.iter (fun def -> 
    match def with
    | DEF_aux (DEF_fundef (FD_aux (FD_function (_, _, funcls), _)), _) -> (*Funcls contains each function definition*)
        (List.iter (*We iterate over each function definition*)
          (fun (FCL_aux (FCL_funcl (id, pexp), _)) ->
            match pexp with
            |Pat_aux (Pat_exp (input, body), _) -> (*Pat_exp contains the input parameters as a pattern and the body of the function*)
              let added_input_env = add_input_to_env input ni_env in (*Parameters are added to the environment*)
              let _ = check_expr env body added_input_env in (*Output and body are checked for non-interference*)
              ()
            | _ -> ())
          funcls) 
    | _ -> ()
  ) ast.defs)  

  (*This function filters out functions included in sail prelude such that we do not analyze them. It works by incrementing a depth counter
  every time we encounter include_start, since all functions herein should be excluded. When we encounter include_end, we return
  to regular depth and add functions.*)
let defs_without_includes defs =
    let rec filter depth acc = function
      | DEF_aux (DEF_pragma ("include_start", _), _) :: rest -> filter (depth + 1) acc rest
      | DEF_aux (DEF_pragma ("include_end", _), _) :: rest -> filter (max 0 (depth - 1)) acc rest
      | def :: rest when depth = 0 -> filter depth (def :: acc) rest
      | _ :: rest -> filter depth acc rest
      | [] -> List.rev acc
    in
    filter 0 [] defs

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
  


  let user_defs = defs_without_includes ast.defs in
  let ni_env = add_functions_to_env env { ast with defs = user_defs } empty_ni_env in
  let ni_env = if !opt_security_level <> None then (
    set_ass_sec_lev ni_env (Option.get !opt_security_level)
  ) else set_ass_sec_lev ni_env Public in
  Printf.printf "Privilege level: %s\n" (string_of_lattice (get_ass_sec_lev ni_env));
  check_ast env { ast with defs = user_defs } ni_env

let _ =
  Target.register
    ~name:"noninterference"
    ~options:noninterference_options
    ~rewrites:[]
    noninterference_target