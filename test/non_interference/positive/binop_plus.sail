default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
    let public_a = 1;
    let public_b = 1;
    let public_c = public_a + public_b;
    return public_c;
}