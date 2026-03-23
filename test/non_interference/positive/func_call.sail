default Order dec
$include <prelude.sail>



val foo : (int) -> int

function foo (public_a) = {
    let public_b : int = public_a;
    return public_b;
} 


val main : unit -> int

function main () = {
    let public_c = 1;
    let public_d = foo(public_c);
    return public_d;
}