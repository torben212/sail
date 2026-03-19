open Libsail
open Ast
open Ast_util
open Ast_compare

type lattice = Public | Secret

module IdMap = Map.Make(struct type t = string let compare = compare end)

type ni_env = {
  lattices : lattice IdMap.t;
  depth : int;
  security_level : lattice;
}

let empty_ni_env = {
  lattices = IdMap.empty;
  depth = 0;
  security_level = Public;
}

let add (id: string) (lat: lattice) (env: ni_env) : ni_env =
  { lattices = IdMap.add id lat env.lattices; depth = env.depth; security_level = env.security_level }

let find (id: string) (env: ni_env) : lattice =
  IdMap.find id env.lattices

let find_opt (id: string) (env: ni_env) : lattice option =
  IdMap.find_opt id env.lattices

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