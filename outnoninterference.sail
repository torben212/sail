val internal_pick = monadic {_: "internal_pick"}: forall ('a : Type).
  list('a) -> 'a

val undefined_bool = monadic {_: "undefined_bool"}: unit -> bool

val undefined_bit = monadic {_: "undefined_bit"}: unit -> bitvector(1)

val undefined_int = monadic {_: "undefined_int"}: unit -> int

val undefined_nat = monadic {_: "undefined_nat"}: unit -> nat

val undefined_real = monadic {_: "undefined_real"}: unit -> real

val undefined_string = monadic {_: "undefined_string"}: unit -> string

val undefined_list = monadic {_: "undefined_list"}: forall ('a : Type).
  'a -> list('a)

val undefined_range = monadic {_: "undefined_range"}: forall ('n : Int) ('m : Int).
  (int('n), int('m)) -> range('n, 'm)

val undefined_vector = monadic {_: "undefined_vector"}: forall ('n : Int) ('a : Type).
  (int('n), 'a) -> vector('n, 'a)

val undefined_bitvector = monadic {_: "undefined_bitvector"}: forall ('n : Int).
  int('n) -> bitvector('n)

val undefined_unit = monadic {_: "undefined_unit"}: unit -> unit

default Order dec

val main : unit -> int

$[complete]
function main () = {
    let public_a = 1;
    let public_b = 1;
    let public_xxx = true;
    let public_yyy = false;
    let public_c = $[overloaded { "name" = "+", "is_infix" = true }] add_atom(public_a, public_b);
    let public_d = $[overloaded { "name" = "-", "is_infix" = true }] sub_atom(public_a, public_b);
    let public_e = $[overloaded { "name" = "*", "is_infix" = true }] mult_atom(public_a, public_b);
    let public_g = $[overloaded { "name" = ">", "is_infix" = true }] gt_int(public_a, public_b);
    let public_h = $[overloaded { "name" = "<", "is_infix" = true }] lt_int(public_a, public_b);
    let public_i = $[overloaded { "name" = "<=", "is_infix" = true }] lteq_int(public_a, public_b);
    let public_j = $[overloaded { "name" = ">=", "is_infix" = true }] gteq_int(public_a, public_b);
    let public_k = $[overloaded { "name" = "==", "is_infix" = true }] eq_int(public_a, public_b);
    let public_l = $[overloaded { "name" = "!=", "is_infix" = true }] neq_int(public_a, public_b);
    let public_m = $[overloaded { "name" = "==", "is_infix" = true }] eq_bool(public_xxx, public_yyy);
    var public_n = $[overloaded { "name" = "!=", "is_infix" = true }] neq_bool(public_xxx, public_yyy);
    public_n = false;
    return(public_c)
}

$[global]
val initialize_registers : unit -> unit

$[complete]
$[global]
function initialize_registers () = ()
