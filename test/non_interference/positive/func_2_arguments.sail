default Order dec
$include <prelude.sail>

val foo : (int, int) -> int

function foo (public_a, public_b) = {
    let public_c : int = public_a + public_b;
    let secret_b : int = public_a;
    return secret_b;
} 


val main : unit -> int

function main () = {
    let public_c = 1;
    let secret_d = foo(public_c, public_c);
    return secret_d;
}