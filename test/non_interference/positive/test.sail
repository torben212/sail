default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
    var public_e = 1;
    let public_a = 1;
    let public_b = 2;
    public_e = public_a;
    let (public_c, public_d) = (public_a, public_b);
    return 1;
}