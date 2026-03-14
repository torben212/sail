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

let rec check_expr env expr = 
  match expr with
  | BinOp(op, e1, e2) -> """Vi skal nok ikke engang bruge binop, men ved ikke om vi har andre regler for high/low når det er bool vs int"""
      check_expr env e1; 
      check_expr env e2;
      match op with
      | Add | Sub | Mul -> ()
      | Div -> ()
      | And | Or -> ()
      | Eq | Neq | Lt | Gt | Leq | Geq -> ()
      | _ -> failwith "Unsupported binary operator"
  | UnOp(op, e) ->
      check_expr env e;
      match op with
      | Neg -> ()
      | Not -> ()
      | _ -> failwith "Unsupported unary operator"
  | Assign(var, value) -> 
      check_expr env value;
      ()
      
  | If(cond, then_branch, else_branch) -> ()
  | While(cond, body) -> ()
  | _ -> ()
  

let check_ast env ast = 
  List.iter (fun def -> 
    match def with
    | FunctionDef(_, _, body) -> check_expr env body
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