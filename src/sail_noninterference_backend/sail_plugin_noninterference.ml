open Libsail
open Interactive.State
open Ast
open Ast_util
open Ast_compare
open Jib
open Jib_util
open Value2
open Printf

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

let is_binop id =
  let op = string_of_id id in
  match op with
  | "+" | "-" | "*" | "/" | "&&" | "||" | "==" | "!=" | "<" | ">" | "<=" | ">=" -> true
  | _ -> false

let rec check_expr (env : Type_check.env) (expr) : unit = 
  match expr with
  | E_aux (E_app (id, [e1; e2]), _) when is_binop id ->
      check_expr env e1;
      check_expr env e2;
      failwith "Binary operator handling not implemented yet"
      (* handle the operator id here *)
  | E_aux (E_lit lit, _) ->
      (* handle literal *)
      failwith "Literal handling not implemented yet"
  | E_aux (E_assign (lexp, value), _) ->
      check_lexp env lexp;
      check_expr env value;
      (* handle assignment *)
  | E_aux (E_id id, _) ->
      (* handle variable *)
      ()
  | E_aux (E_if (cond, then_exp, else_exp), _) ->
      check_expr env cond;
      check_expr env then_exp;
      check_expr env else_exp;
      (* handle if statement *)
  | _ -> ()

and check_lexp (env : Type_check.env) (lexp) : unit =
  match lexp with
  | LE_aux (LE_id id, _) -> ()
  | LE_aux (LE_deref exp, _) -> check_expr env exp
  | LE_aux (LE_field (lexp, _), _) -> check_lexp env lexp
  | _ -> ()

let check_ast (env : Type_check.env) (ast : Type_check.typed_ast) = 
  List.iter (fun def -> 
    match def with
    | DEF_aux (DEF_fundef (FD_aux (FD_function (_, _, funcls), _)), _) ->
        List.iter 
          (fun (FCL_aux (FCL_funcl (_, pexp), _)) ->
            match pexp with
            |Pat_aux (Pat_exp (_, body), _) -> check_expr env body
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
  
  check_ast env ast

let _ =
  Target.register
    ~name:"noninterference"
    ~options:noninterference_options
    ~rewrites:[]
    noninterference_target