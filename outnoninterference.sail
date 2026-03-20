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

val foo : int -> int

$[complete]
function foo public_a = {
    let public_b : int = public_a;
    return(public_b)
}

val main : unit -> int

$[complete]
function main () = {
    let public_c = 1;
    let public_d = foo(public_c);
    return(public_d)
}

$[global]
val initialize_registers : unit -> unit

$[complete]
$[global]
function initialize_registers () = ()
