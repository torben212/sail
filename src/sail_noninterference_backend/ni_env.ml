open Libsail
open Ast
open Ast_util
open Ast_compare

type lattice = Public | Secret

module IdMap = Map.Make(struct type t = id let compare = compare end)

type ni_env = {
  lattices : lattice IdMap.t;
}

let empty_ni_env = {
  lattices = IdMap.empty;
}

let add (id: id) (lat: lattice) (env: ni_env) : ni_env =
  { lattices = IdMap.add id lat env.lattices }

let find (id: id) (env: ni_env) : lattice =
  IdMap.find id env.lattices

let find_opt (id: id) (env: ni_env) : lattice option =
  IdMap.find_opt id env.lattices