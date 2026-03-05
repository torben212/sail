open Libsail
open Interactive.State
open Ast
open Ast_util
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
  
  (* Your noninterference analysis implementation goes here *)
  (* This function should return unit () *)
  failwith "not yet implemented"

let _ =
  Target.register
    ~name:"noninterference"
    ~options:noninterference_options
    ~rewrites:[]
    noninterference_target