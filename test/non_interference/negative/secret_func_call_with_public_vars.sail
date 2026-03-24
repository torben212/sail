default Order dec
$include <prelude.sail>

val foo : (int) -> int

function foo (secret_a) = {
    let secret_b : int = secret_a;
    return secret_b;
} 


val main : unit -> int

function main () = {
    let public_c = 1;
    let public_d = foo(public_c);
    return public_d;
}