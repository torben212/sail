default Order dec
$include <prelude.sail>



val foo : (int) -> int

function foo (public_a) = {
    let secret_b : int = public_a;
    return secret_b;
} 


val main : unit -> int

function main () = {
    let public_c = 1;
    let secret_d = foo(public_c);
    return secret_d;
}