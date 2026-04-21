open Libsail
open Ast
open Ast_util
open Ast_compare

type lattice = Public | Secret
type mutability = Mutable | Fragile
type literal =
| Int of int 
| String of string

module IdMap = Map.Make(struct type t = string let compare = compare end)

type dc_env = {
  lattices : lattice IdMap.t;
  mutability : mutability IdMap.t;
  functions : string list;
  depth : int;
  security_level : lattice;
  variables : string IdMap.t
}

let empty_dc_env = {
  lattices = IdMap.empty;
  mutability = IdMap.empty;
  functions = [];
  depth = 0;
  security_level = Public;
  variables = IdMap.empty;
}

let add (id: string) (lat: lattice) (env: dc_env) : dc_env =
  { env with lattices = IdMap.add id lat env.lattices }
let find (id: string) (env: dc_env) : lattice =
  IdMap.find id env.lattices

let find_opt (id: string) (env: dc_env) : lattice option =
  IdMap.find_opt id env.lattices

let find_mutability (id: string) (env: dc_env) : mutability =
  IdMap.find id env.mutability
let find_mutability_opt (id: string) (env: dc_env) : mutability option =
  IdMap.find_opt id env.mutability
let add_mutability (id: string) (mut: mutability) (env: dc_env) : dc_env =
  { env with mutability = IdMap.add id mut env.mutability }
let check_security_level (env: dc_env) : lattice =
  env.security_level

let set_security_level (env : dc_env) (lat : lattice) : dc_env =
  {env with security_level = lat}

let increase_depth (env : dc_env) : dc_env =
  {env with depth = env.depth + 1}

let decrease_depth (env : dc_env) : dc_env = 
  {env with depth = env.depth - 1}
let get_depth (env : dc_env) : int =
  env.depth

let add_function (id: string) (env: dc_env) : dc_env =
  {env with functions = id :: env.functions}

let find_function (id: string) (env: dc_env) : (string list) =
  env.functions

let add_variable (id:string) (value: string) (env: dc_env): dc_env =
  {env with variables = IdMap.add id value env.variables}

let find_variable_opt (id: string) (env: dc_env) : string option =
  IdMap.find_opt id env.variables