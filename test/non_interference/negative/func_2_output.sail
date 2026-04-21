default Order dec
$include <prelude.sail>

val foo : (int, int) -> (int, int)

function foo (public_a, secret_a) = {
    let public_c : int = public_a + public_a;
    let secret_b : int = secret_a;
    let secret_c : int = 2;
    return (secret_b, secret_c);
} 


val main : unit -> int

function main () = {
    let public_c = 1;
    let secret_a = 1;
    let (secret_d, public_e) = foo(public_c, secret_a);
    return secret_d;
}