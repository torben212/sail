default Order dec
$include <prelude.sail>

val foo : int -> int

$[complete]
function foo secret_a = {
    let secret_b = 1;
    if $[overloaded { "name" = ">", "is_infix" = true }] gt_int(secret_b, 0) then {
        let public_c = secret_a;
        ()
    } else if $[overloaded { "name" = "==", "is_infix" = true }] eq_int(secret_b, $[overloaded { "name" = "negate", "is_infix" = false }] negate_atom(1)) then {
        let public_c = 1;
        ()
    } else {
        let public_c = 1;
        ()
    };
    return(1)
}

val main : unit -> int

$[complete]
function main () = {
    return(1)
}
