default Order dec
$include <prelude.sail>

val main : unit -> int

val foo : ref (int) -> int

function foo (public_c) = {
    public_c = 2;
    return public_c;
}

function main () = {
    var public_a : int = 1;
    let secret_b = 1;
    public_a = secret_b;
    return return(public_c);
}