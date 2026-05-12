open Libsail
open Ast
open Ast_util
open Ast_compare

type lattice = Public | Secret | User | Supervisor | Machine
type mutability = Mutable | Fragile

module IdMap = Map.Make(struct type t = string let compare = compare end)

type ni_env = {
  lattices : lattice IdMap.t;
  mutability : mutability IdMap.t;
  functions : (lattice list * lattice list) IdMap.t;
  depth : int;
  security_level : lattice;
  ass_sec_lev : lattice;
}

let empty_ni_env = {
  lattices = IdMap.empty;
  mutability = IdMap.empty;
  functions = IdMap.empty;
  depth = 0;
  security_level = Public;
  ass_sec_lev = User;
}

let add (id: string) (lat: lattice) (env: ni_env) : ni_env =
  { lattices = IdMap.add id lat env.lattices; mutability = env.mutability; functions = env.functions; depth = env.depth; security_level = env.security_level; ass_sec_lev = env.ass_sec_lev}

let find (id: string) (env: ni_env) : lattice =
  IdMap.find id env.lattices

let find_opt (id: string) (env: ni_env) : lattice option =
  IdMap.find_opt id env.lattices

let find_mutability (id: string) (env: ni_env) : mutability =
  IdMap.find id env.mutability
let find_mutability_opt (id: string) (env: ni_env) : mutability option =
  IdMap.find_opt id env.mutability
let add_mutability (id: string) (mut: mutability) (env: ni_env) : ni_env =
  { env with mutability = IdMap.add id mut env.mutability }
let check_security_level (env: ni_env) : lattice =
  env.security_level

let set_security_level (env : ni_env) (lat : lattice) : ni_env =
  {env with security_level = lat}

let increase_depth (env : ni_env) : ni_env =
  {env with depth = env.depth + 1}

let decrease_depth (env : ni_env) : ni_env = 
  {env with depth = env.depth - 1}
let get_depth (env : ni_env) : int =
  env.depth

let add_function (id: string) (input_lat : lattice list) (output_lat : lattice list) (env: ni_env) : ni_env =
  {env with functions = IdMap.add id (input_lat, output_lat) env.functions}

let find_function (id: string) (env: ni_env) : (lattice list * lattice list) =
  IdMap.find id env.functions

let get_ass_sec_lev (env : ni_env) : lattice =
  env.ass_sec_lev

let set_ass_sec_lev (env : ni_env) (lat : lattice) : ni_env =
  {env with ass_sec_lev = lat}