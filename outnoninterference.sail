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

$include_start /home/torben/.opam/default/bin/../share/sail/lib/prelude.sail

$include_start /home/torben/.opam/default/bin/../share/sail/lib/flow.sail

type bit = bitvector(1)

val eq_unit = pure {lean: "_lean_beq", _: "eq_unit"}: (unit, unit) -> bool(true)

$[complete]
function eq_unit (_, _) = true

val not_bool = pure {coq: "negb", lean: "_lean_not", _: "not"}: forall ('p : Bool).
  bool('p) -> bool(not('p))

val and_bool = pure {coq: "andb", lean: "_lean_and", _: "and_bool"}: forall ('p : Bool) ('q : Bool).
  (bool('p), bool('q)) -> bool(('p & 'q))

val and_bool_no_flow = pure {coq: "andb", lean: "_lean_and", _: "and_bool"}: (bool, bool) -> bool

val or_bool = pure {coq: "orb", lean: "_lean_or", _: "or_bool"}: forall ('p : Bool) ('q : Bool).
  (bool('p), bool('q)) -> bool(('p | 'q))

val eq_int = pure {ocaml: "eq_int", interpreter: "eq_int", lem: "eq", coq: "Z.eqb", lean: "_lean_beq", _: "eq_int"}: forall ('n : Int) ('m : Int).
  (int('n), int('m)) -> bool('n == 'm)

val eq_bool = pure {ocaml: "eq_bool", interpreter: "eq_bool", lem: "eq", coq: "Bool.eqb", lean: "_lean_beq", _: "eq_bool"}: (bool, bool) -> bool

val neq_int = pure {lem: "neq", lean: "_lean_bne"}: forall ('n : Int) ('m : Int).
  (int('n), int('m)) -> bool('n != 'm)

$[complete]
function neq_int (x, y) = not_bool(eq_int(x, y))

val neq_bool : (bool, bool) -> bool

$[complete]
function neq_bool (x, y) = not_bool(eq_bool(x, y))

val lteq_int = pure {coq: "Z.leb", lean: "_lean_le", _: "lteq"}: forall ('n : Int) ('m : Int).
  (int('n), int('m)) -> bool('n <= 'm)

val gteq_int = pure {coq: "Z.geb", lean: "_lean_ge", _: "gteq"}: forall ('n : Int) ('m : Int).
  (int('n), int('m)) -> bool('n >= 'm)

val lt_int = pure {coq: "Z.ltb", lean: "_lean_lt", _: "lt"}: forall ('n : Int) ('m : Int).
  (int('n), int('m)) -> bool('n < 'm)

val gt_int = pure {coq: "Z.gtb", lean: "_lean_gt", _: "gt"}: forall ('n : Int) ('m : Int).
  (int('n), int('m)) -> bool('n > 'm)

overload operator == = {eq_int, eq_bool, eq_unit}

overload operator != = {neq_int, neq_bool}

overload operator | = {or_bool}

overload operator & = {and_bool}

overload operator <= = {lteq_int}

overload operator < = {lt_int}

overload operator >= = {gteq_int}

overload operator > = {gt_int}

val __id = pure {systemverilog: "id"}: forall ('n : Int). int('n) -> int('n)

$[complete]
function __id x = x

overload __size = {__id}

val __deref = monadic {_: "reg_deref"}: forall ('a : Type). register('a) -> 'a

$include_end /home/torben/.opam/default/bin/../share/sail/lib/flow.sail

$include_start /home/torben/.opam/default/bin/../share/sail/lib/vector.sail

$include_start /home/torben/.opam/default/bin/../share/sail/lib/arith.sail

val add_atom = pure {ocaml: "add_int", interpreter: "add_int", lem: "integerAdd", coq: "Z.add", lean: "_lean_addi", _: "add_int"}: forall ('n : Int) ('m : Int).
  (int('n), int('m)) -> int('n + 'm)

val add_int = pure {ocaml: "add_int", interpreter: "add_int", lem: "integerAdd", coq: "Z.add", lean: "_lean_addi", _: "add_int"}: (int, int) -> int

overload operator + = {add_atom, add_int}

val sub_atom = pure {ocaml: "sub_int", interpreter: "sub_int", lem: "integerMinus", coq: "Z.sub", lean: "_lean_subi", _: "sub_int"}: forall ('n : Int) ('m : Int).
  (int('n), int('m)) -> int('n - 'm)

val sub_int = pure {ocaml: "sub_int", interpreter: "sub_int", lem: "integerMinus", coq: "Z.sub", lean: "_lean_subi", _: "sub_int"}: (int, int) -> int

overload operator - = {sub_atom, sub_int}

val sub_nat = pure {ocaml: "(fun (x,y) -> let n = sub_int (x,y) in if Big_int.less_equal n Big_int.zero then Big_int.zero else n)", lem: "integerMinus", coq: "Z.sub", lean: "_lean_subi", _: "sub_nat"}: (nat, nat) -> nat

val negate_atom = pure {ocaml: "negate", interpreter: "negate", lem: "integerNegate", coq: "Z.opp", lean: "Neg.neg", _: "neg_int"}: forall ('n : Int).
  int('n) -> int(- 'n)

val negate_int = pure {ocaml: "negate", interpreter: "negate", lem: "integerNegate", coq: "Z.opp", lean: "Neg.neg", _: "neg_int"}: int -> int

overload negate = {negate_atom, negate_int}

val mult_atom = pure {ocaml: "mult", interpreter: "mult", lem: "integerMult", coq: "Z.mul", lean: "_lean_muli", _: "mult_int"}: forall ('n : Int) ('m : Int).
  (int('n), int('m)) -> int('n * 'm)

val mult_int = pure {ocaml: "mult", interpreter: "mult", lem: "integerMult", coq: "Z.mul", lean: "_lean_muli", _: "mult_int"}: (int, int) -> int

overload operator * = {mult_atom, mult_int}

$[sv_module { "stdout" = true }]
val print_int = pure {_: "print_int"}: (string, int) -> unit

$[sv_module { "stderr" = true }]
val prerr_int = pure {_: "prerr_int"}: (string, int) -> unit

/*!
We have special support for raising values to the power of two. Any Sail expression `2 ^ x` will be compiled to this builtin.
*/
val pow2 = pure {lean: "_lean_pow2i", _: "pow2"}: forall ('n : Int).
  int('n) -> int(2 ^ 'n)

/*!
A common idiom in asl is to take two bits of an opcode and convert in into a variable like
```
let elsize = shl_int(8, UInt(size))
```
THIS ensures that in this case the typechecker knows that the end result will be a value in the set `{8, 16, 32, 64}`

Similarly, we define shifts of 32 and 1 (i.e., powers of two).

The most general shift operations also allow negative shifts which go in the opposite direction, for compatibility with ASL.
*/
val _shl8 = pure {c: "shl_mach_int", cpp: "shl_mach_int", lean: "Int.shiftl", _: "shl_int"}: forall ('n : Int), (0 <= 'n & 'n <= 3).
  (int(8), int('n)) -> {8, 16, 32, 64}

val _shl32 = pure {c: "shl_mach_int", cpp: "shl_mach_int", lean: "Int.shiftl", _: "shl_int"}: forall ('n : Int), 'n in {0, 1}.
  (int(32), int('n)) -> {32, 64}

val _shl1 = pure {c: "shl_mach_int", cpp: "shl_mach_int", lean: "Int.shiftl", _: "shl_int"}: forall ('n : Int), (0 <= 'n & 'n <= 3).
  (int(1), int('n)) -> {1, 2, 4, 8}

val _shl_int = pure {lean: "Int.shiftl", _: "shl_int"}: forall ('n : Int), 0 <= 'n.
  (int, int('n)) -> int

val _shr32 = pure {c: "shr_mach_int", cpp: "shr_mach_int", lean: "Int.shiftl", _: "shr_int"}: forall ('n : Int), (0 <= 'n & 'n <= 31).
  (int('n), int(1)) -> {('m : Int), (0 <= 'm & 'm <= 15). int('m)}

val _shr_int = pure {lean: "Int.shiftr", _: "shr_int"}: forall ('n : Int), 0 <= 'n.
  (int, int('n)) -> int

val _shl_int_general : (int, int) -> int

$[complete]
function _shl_int_general (m : int, n : int) = if $[overloaded { "name" = ">=", "is_infix" = true }] gteq_int(n, 0) then
  _shl_int(m, n)
else
  _shr_int(m, $[overloaded { "name" = "negate", "is_infix" = false }] negate_atom(n))

val _shr_int_general : (int, int) -> int

$[complete]
function _shr_int_general (m : int, n : int) = if $[overloaded { "name" = ">=", "is_infix" = true }] gteq_int(n, 0) then
  _shr_int(m, n)
else
  _shl_int(m, $[overloaded { "name" = "negate", "is_infix" = false }] negate_atom(n))

overload shl_int = {_shl1, _shl8, _shl32, _shl_int, _shl_int_general}

overload shr_int = {_shr32, _shr_int, _shr_int_general}

/*! Truncating division (rounds towards zero) */
val tdiv_int = pure {coq: "Z.quot", lean: "Int.tdiv", _: "tdiv_int"}: (int, int) -> int

/*! Remainder for truncating division (has sign of dividend) */
val _tmod_int = pure {coq: "Z.rem", lean: "Int.tmod", _: "tmod_int"}: (int, int) -> int

/*! If we know the second argument is positive, we know the result is positive */
val _tmod_int_positive = pure {ocaml: "tmod_int", interpreter: "tmod_int", lem: "tmod_int", coq: "Z.rem", lean: "Int.tmod", _: "tmod_int"}: forall ('n : Int), 'n >= 1.
  (int, int('n)) -> nat

overload tmod_int = {_tmod_int_positive, _tmod_int}

$c_reserved fdiv_int

val fdiv_int : (int, int) -> int

$[complete]
function fdiv_int (n : int, m : int) = {
    if $[overloaded { "name" = "&", "is_infix" = true }] and_bool($[overloaded { "name" = "<", "is_infix" = true }] lt_int(n, 0), $[overloaded { "name" = ">", "is_infix" = true }] gt_int(m, 0)) then {
        $[overloaded { "name" = "-", "is_infix" = true }] sub_atom(tdiv_int($[overloaded { "name" = "+", "is_infix" = true }] add_atom(n, 1), m), 1)
    } else if $[overloaded { "name" = "&", "is_infix" = true }] and_bool($[overloaded { "name" = ">", "is_infix" = true }] gt_int(n, 0), $[overloaded { "name" = "<", "is_infix" = true }] lt_int(m, 0)) then {
        $[overloaded { "name" = "-", "is_infix" = true }] sub_atom(tdiv_int($[overloaded { "name" = "-", "is_infix" = true }] sub_atom(n, 1), m), 1)
    } else {
        tdiv_int(n, m)
    }
}

$c_reserved fmod_int

val fmod_int : (int, int) -> int

$[complete]
function fmod_int (n : int, m : int) = {
    $[overloaded { "name" = "-", "is_infix" = true }] sub_atom(n, $[overloaded { "name" = "*", "is_infix" = true }] mult_atom(m, fdiv_int(n, m)))
}

val abs_int_plain = pure {smt: "abs", ocaml: "abs_int", interpreter: "abs_int", lem: "integerAbs", coq: "Z.abs", lean: "Sail.Int.intAbs", _: "abs_int"}: int -> int

overload abs_int = {abs_int_plain}

val max_int = pure {lem: "integerMax", coq: "Z.max", lean: "Max.max", _: "max_int"}: forall ('x : Int) ('y : Int).
  (int('x), int('y)) -> {('z : Int), ('x >= 'y & 'z == 'x | 'x < 'y & 'z == 'y). int('z)}

val min_int = pure {lem: "integerMin", coq: "Z.min", lean: "Min.min", _: "min_int"}: forall ('x : Int) ('y : Int).
  (int('x), int('y)) -> {('z : Int), ('x < 'y & 'z == 'x | 'x >= 'y & 'z == 'y). int('z)}

$include_end /home/torben/.opam/default/bin/../share/sail/lib/arith.sail

type bits('n: Int) = bitvector('n)

val eq_bit = pure {ocaml: "eq_list", interpreter: "eq_list", lem: "eq_vec", coq: "eq_vec", lean: "_lean_beq", _: "eq_bits"}: (bitvector(1), bitvector(1)) -> bool

val eq_bits = pure {ocaml: "eq_list", interpreter: "eq_list", lem: "eq_vec", coq: "eq_vec", lean: "_lean_beq", _: "eq_bits"}: forall ('n : Int).
  (bitvector('n), bitvector('n)) -> bool

overload operator == = {eq_bits}

val neq_bits = pure {lem: "neq_vec", coq: "neq_vec", c: "neq_bits", cpp: "neq_bits", lean: "_lean_bne"}: forall ('n : Int).
  (bitvector('n), bitvector('n)) -> bool

$[complete]
function neq_bits (x, y) = not_bool(eq_bits(x, y))

overload operator != = {neq_bits}

val bitvector_length = pure {coq: "length_mword", lean: "Sail.BitVec.length", _: "length"}: forall ('n : Int).
  bitvector('n) -> int('n)

val vector_length = pure {ocaml: "length", interpreter: "length", lem: "length_list", coq: "vec_length", lean: "Vector.length", _: "length"}: forall ('n : Int) ('a : Type).
  vector('n, 'a) -> int('n)

val vector_init = pure {lean: "vectorInit", _: "vector_init"}: forall ('n : Int) ('a : Type), 'n >= 0.
  (implicit('n), 'a) -> vector('n, 'a)

overload length = {bitvector_length, vector_length}

val count_leading_zeros = pure {lean: "BitVec.countLeadingZeros", _: "count_leading_zeros"}: forall ('N : Int), 'N >= 1.
  bitvector('N) -> {('n : Int), (0 <= 'n & 'n <= 'N). int('n)}

val count_trailing_zeros = pure {lean: "BitVec.countTrailingZeros", _: "count_trailing_zeros"}: forall ('N : Int), 'N >= 1.
  bitvector('N) -> {('n : Int), (0 <= 'n & 'n <= 'N). int('n)}

$[sv_module { "stdout" = true }]
val print_bits = pure {_: "print_bits"}: forall ('n : Int).
  (string, bitvector('n)) -> unit

$[sv_module { "stderr" = true }]
val prerr_bits = pure {_: "prerr_bits"}: forall ('n : Int).
  (string, bitvector('n)) -> unit

val sail_sign_extend = pure {lean: "Sail.BitVec.signExtend", _: "sign_extend"}: forall ('n : Int) ('m : Int), 'm >= 'n.
  (bitvector('n), int('m)) -> bitvector('m)

val sail_zero_extend = pure {lean: "Sail.BitVec.zeroExtend", _: "zero_extend"}: forall ('n : Int) ('m : Int), 'm >= 'n.
  (bitvector('n), int('m)) -> bitvector('m)

/*!
THIS`(v, n)` truncates `v`, keeping only the _least_ significant `n` bits.
 */
val truncate = pure {ocaml: "vector_truncate", interpreter: "vector_truncate", lem: "vector_truncate", coq: "vector_truncate", lean: "Sail.BitVec.truncate", _: "sail_truncate"}: forall ('m : Int) ('n : Int), ('m >= 0 & 'm <= 'n).
  (bitvector('n), int('m)) -> bitvector('m)

/*!
THIS`(v, n)` truncates `v`, keeping only the _most_ significant `n` bits.
 */
val truncateLSB = pure {ocaml: "vector_truncateLSB", interpreter: "vector_truncateLSB", lem: "vector_truncateLSB", coq: "vector_truncateLSB", lean: "Sail.BitVec.truncateLsb", _: "sail_truncateLSB"}: forall ('m : Int) ('n : Int), ('m >= 0 & 'm <= 'n).
  (bitvector('n), int('m)) -> bitvector('m)

val sail_mask : forall ('len : Int) ('v : Int), ('len >= 0 & 'v >= 0).
  (int('len), bitvector('v)) -> bitvector('len)

$[complete]
function sail_mask (len, v) = if $[overloaded { "name" = "<=", "is_infix" = true }] lteq_int(len, $[overloaded { "name" = "length", "is_infix" = false }] bitvector_length(v)) then
  truncate(v, len)
else
  sail_zero_extend(v, len)

overload operator ^ = {sail_mask}

infix 8 =^

overload operator =^ = {sail_mask}

val bitvector_concat = pure {ocaml: "append", interpreter: "append", lem: "concat_vec", coq: "concat_vec", lean: "_lean_app", _: "append"}: forall ('n : Int) ('m : Int).
  (bitvector('n), bitvector('m)) -> bitvector('n + 'm)

overload append = {bitvector_concat}

val append_64 = pure {_: "append_64"}: forall ('n : Int).
  (bitvector('n), bitvector(64)) -> bitvector('n + 64)

val bitvector_access = pure {ocaml: "access", interpreter: "access", lem: "access_vec_dec", coq: "access_vec_dec", lean: "BitVec.access", _: "vector_access"}: forall ('n : Int) ('m : Int), (0 <= 'm & 'm < 'n).
  (bitvector('n), int('m)) -> bitvector(1)

val plain_vector_access = pure {ocaml: "access_list", interpreter: "access", lem: "access_list_dec", coq: "vec_access_dec", lean: "GetElem?.getElem!", _: "vector_access"}: forall ('n : Int) ('m : Int) ('a : Type), (0 <= 'm & 'm < 'n).
  (vector('n, 'a), int('m)) -> 'a

overload vector_access = {bitvector_access, plain_vector_access}

val bitvector_update = pure {ocaml: "update", interpreter: "update", lem: "update_vec_dec", coq: "update_vec_dec", lean: "BitVec.update", _: "vector_update"}: forall ('n : Int) ('m : Int), (0 <= 'm & 'm < 'n).
  (bitvector('n), int('m), bitvector(1)) -> bitvector('n)

val plain_vector_update = pure {ocaml: "update_list", interpreter: "update", lem: "update_list_dec", coq: "vec_update_dec", lean: "vectorUpdate", _: "vector_update"}: forall ('n : Int) ('m : Int) ('a : Type), (0 <= 'm & 'm < 'n).
  (vector('n, 'a), int('m), 'a) -> vector('n, 'a)

overload vector_update = {bitvector_update, plain_vector_update}

val add_bits = pure {ocaml: "add_vec", interpreter: "add_vec", lem: "add_vec", coq: "add_vec", lean: "_lean_add", _: "add_bits"}: forall ('n : Int).
  (bitvector('n), bitvector('n)) -> bitvector('n)

val add_bits_int = pure {ocaml: "add_vec_int", interpreter: "add_vec_int", lem: "add_vec_int", coq: "add_vec_int", lean: "BitVec.addInt", _: "add_bits_int"}: forall ('n : Int).
  (bitvector('n), int) -> bitvector('n)

overload operator + = {add_bits, add_bits_int}

val sub_bits = pure {ocaml: "sub_vec", interpreter: "sub_vec", lem: "sub_vec", coq: "sub_vec", lean: "_lean_sub", _: "sub_bits"}: forall ('n : Int).
  (bitvector('n), bitvector('n)) -> bitvector('n)

val not_vec = pure {ocaml: "not_vec", lem: "not_vec", coq: "not_vec", interpreter: "not_vec", lean: "Complement.complement", _: "not_bits"}: forall ('n : Int).
  bitvector('n) -> bitvector('n)

val and_vec = pure {lem: "and_vec", coq: "and_vec", ocaml: "and_vec", interpreter: "and_vec", lean: "_lean_bvand", _: "and_bits"}: forall ('n : Int).
  (bitvector('n), bitvector('n)) -> bitvector('n)

overload operator & = {and_vec}

val or_vec = pure {lem: "or_vec", coq: "or_vec", ocaml: "or_vec", interpreter: "or_vec", lean: "_lean_bvor", _: "or_bits"}: forall ('n : Int).
  (bitvector('n), bitvector('n)) -> bitvector('n)

overload operator | = {or_vec}

val xor_vec = pure {lem: "xor_vec", coq: "xor_vec", ocaml: "xor_vec", interpreter: "xor_vec", lean: "_lean_bvxor", _: "xor_bits"}: forall ('n : Int).
  (bitvector('n), bitvector('n)) -> bitvector('n)

val subrange_bits = pure {ocaml: "subrange", interpreter: "subrange", lem: "subrange_vec_dec", coq: "subrange_vec_dec", lean: "Sail.BitVec.extractLsb", _: "vector_subrange"}: forall ('n : Int) ('m : Int) ('o : Int), (0 <= 'o & 'o <= 'm & 'm < 'n).
  (bitvector('n), int('m), int('o)) -> bitvector('m - 'o + 1)

overload vector_subrange = {subrange_bits}

val update_subrange_bits = pure {ocaml: "update_subrange", interpreter: "update_subrange", lem: "update_subrange_vec_dec", coq: "update_subrange_vec_dec", lean: "Sail.BitVec.updateSubrange", _: "vector_update_subrange"}: forall ('n : Int) ('m : Int) ('o : Int), (0 <= 'o & 'o <= 'm & 'm < 'n).
  (bitvector('n), int('m), int('o), bitvector('m - ('o - 1))) -> bitvector('n)

overload vector_update_subrange = {update_subrange_bits}

val sail_shiftleft = pure {lean: "_lean_shiftl", _: "shiftl"}: forall ('n : Int).
  (bitvector('n), int) -> bitvector('n)

val sail_shiftright = pure {lean: "_lean_shiftr", _: "shiftr"}: forall ('n : Int).
  (bitvector('n), int) -> bitvector('n)

val sail_arith_shiftright = pure {lean: "BitVec.sshiftRight", _: "arith_shiftr"}: forall ('n : Int).
  (bitvector('n), int) -> bitvector('n)

val sail_zeros = pure {lean: "BitVec.zero", _: "zeros"}: forall ('n : Int), 'n >= 0.
  int('n) -> bitvector('n)

val sail_ones : forall ('n : Int), 'n >= 0. int('n) -> bitvector('n)

$[complete]
function sail_ones n = not_vec(sail_zeros(n))

val slice = pure {lean: "BitVec.slice", _: "slice"}: forall ('n : Int) ('m : Int) ('o : Int), (0 <= 'm & 0 <= 'n).
  (bitvector('m), int('o), int('n)) -> bitvector('n)

val replicate_bits = pure {lean: "BitVec.replicateBits", _: "replicate_bits"}: forall ('n : Int) ('m : Int), 'm >= 0.
  (bitvector('n), int('m)) -> bitvector('n * 'm)

val slice_mask : forall ('n : Int), 'n >= 0.
  (implicit('n), int, int) -> bitvector('n)

$[complete]
function slice_mask (n, i, l) = if $[overloaded { "name" = ">=", "is_infix" = true }] gteq_int(l, n) then {
    sail_shiftleft(sail_ones(n), i)
} else {
    let one : bits('n) = sail_mask(n, [0b1] : bits(1));
    sail_shiftleft(sub_bits(sail_shiftleft(one, l), one), i)
}

val get_slice_int = pure {_: "get_slice_int"}: forall ('w : Int).
  (int('w), int, int) -> bitvector('w)

val set_slice_int = pure {_: "set_slice_int"}: forall ('w : Int).
  (int('w), int, int, bitvector('w)) -> int

val set_slice_bits = pure {_: "set_slice"}: forall ('n : Int) ('m : Int).
  (implicit('n), int('m), bitvector('n), int, bitvector('m)) -> bitvector('n)

/*!
converts a bit vector of length $n$ to an integer in the range $0$ to $2^n - 1$.
 */
val unsigned = pure {ocaml: "uint", lem: "uint", interpreter: "uint", coq: "uint", lean: "BitVec.toNatInt", _: "sail_unsigned"}: forall ('n : Int).
  bitvector('n) -> range(0, 2 ^ 'n - 1)

/*!
converts a bit vector of length $n$ to an integer in the range $-2^{n-1}$ to $2^{n-1} - 1$ using twos-complement.
 */
val signed = pure {ocaml: "sint", lem: "sint", interpreter: "sint", coq: "sint", lean: "BitVec.toInt", _: "sail_signed"}: forall ('n : Int), 'n > 0.
  bitvector('n) -> range(- (2 ^ ('n - 1)), 2 ^ ('n - 1) - 1)

overload __size = {__id, bitvector_length, vector_length}

val to_bytes_le : forall ('n : Int), 'n > 0.
  (implicit('n), bitvector(8 * 'n)) -> vector('n, bitvector(8))

$[complete]
function to_bytes_le (n, b) = {
    var res = vector_init(n, sail_zeros(8));
    foreach (i
    from 0
    to $[overloaded { "name" = "-", "is_infix" = true }] sub_atom(n, 1)
    by 1
    in inc) {
        res[i] = $[overloaded { "name" = "vector_subrange", "is_infix" = false }] $[notation { "level" = 0, "syntax" = [10, "[", 0, " .. ", 0, "]"] }] subrange_bits(b, $[overloaded { "name" = "+", "is_infix" = true }] add_atom($[overloaded { "name" = "*", "is_infix" = true }] mult_atom(8, i), 7), $[overloaded { "name" = "*", "is_infix" = true }] mult_atom(8, i))
    };
    res
}

val from_bytes_le : forall ('n : Int), 'n > 0.
  (implicit('n), vector('n, bitvector(8))) -> bitvector(8 * 'n)

$[complete]
function from_bytes_le (n, v) = {
    var res = sail_zeros($[overloaded { "name" = "*", "is_infix" = true }] mult_atom(8, n));
    foreach (i
    from 0
    to $[overloaded { "name" = "-", "is_infix" = true }] sub_atom(n, 1)
    by 1
    in inc) {
        res[$[overloaded { "name" = "+", "is_infix" = true }] add_atom($[overloaded { "name" = "*", "is_infix" = true }] mult_atom(8, i), 7) .. $[overloaded { "name" = "*", "is_infix" = true }] mult_atom(8, i)] = $[overloaded { "name" = "vector_access", "is_infix" = false }] $[notation { "level" = 0, "syntax" = [10, "[", 0, "]"] }] plain_vector_access(v, i)
    };
    res
}

$include_end /home/torben/.opam/default/bin/../share/sail/lib/vector.sail

$include_start /home/torben/.opam/default/bin/../share/sail/lib/option.sail

$sail_internal 

union option ('a: Type) = {Some : 'a, None : unit}

val is_none : forall ('a : Type). option('a) -> bool

$[complete]
function is_none opt = $[complete] match opt {
  Some(_) => false,
  None(()) => true
}

val is_some : forall ('a : Type). option('a) -> bool

$[complete]
function is_some opt = $[complete] match opt {
  Some(_) => true,
  None(()) => false
}

$include_end /home/torben/.opam/default/bin/../share/sail/lib/option.sail

$include_start /home/torben/.opam/default/bin/../share/sail/lib/string.sail

val eq_string = pure {lem: "eq", coq: "generic_eq", lean: "_lean_beq", _: "eq_string"}: (string, string) -> bool

overload operator == = {eq_string}

val concat_str = pure {coq: "String.append", lem: "stringAppend", lean: "HAppend.hAppend", _: "concat_str"}: (string, string) -> string

val dec_str = pure {lean: "Int.repr", _: "dec_str"}: int -> string

val hex_str = pure {lean: "Int.toHex", _: "hex_str"}: int -> string

val hex_str_upper = pure {lean: "Int.toHexUpper", _: "hex_str_upper"}: int -> string

val bits_str = pure {lean: "BitVec.toFormatted", _: "string_of_bits"}: forall ('n : Int).
  bitvector('n) -> string

val concat_str_bits : forall ('n : Int). (string, bitvector('n)) -> string

$[complete]
function concat_str_bits (str, x) = concat_str(str, bits_str(x))

val concat_str_dec : (string, int) -> string

$[complete]
function concat_str_dec (str, x) = concat_str(str, dec_str(x))

$[sv_module { "stdout" = true }]
val print_endline = pure {_: "print_endline"}: string -> unit

$[sv_module { "stderr" = true }]
val prerr_endline = pure {_: "prerr_endline"}: string -> unit

$[sv_module { "stdout" = true }]
val print = pure {_: "print"}: string -> unit

$[sv_module { "stderr" = true }]
val prerr = pure {_: "prerr"}: string -> unit

$include_end /home/torben/.opam/default/bin/../share/sail/lib/string.sail

$include_end /home/torben/.opam/default/bin/../share/sail/lib/prelude.sail

val main : unit -> int

$[complete]
function main () = {
    let secret_x : int = 0;
    let secret_y : int = 5;
    let public_z : int = $[overloaded { "name" = "+", "is_infix" = true }] add_atom(secret_x, secret_y);
    return(public_z)
}

$[global]
val initialize_registers : unit -> unit

$[complete]
$[global]
function initialize_registers () = ()
