default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
    let public_a = 1;
    let public_b = 1;
    let public_xxx = true;
    let public_yyy = false;
    let public_c = public_a + public_b;
    let public_d = public_a - public_b;
    let public_e = public_a * public_b;
    let public_g = public_a > public_b;
    let public_h = public_a < public_b;
    let public_i = public_a <= public_b;
    let public_j = public_a >= public_b;
    let public_k = public_a == public_b;
    let public_l = public_a != public_b;
    let public_m = public_xxx == public_yyy;
    var public_n = public_xxx != public_yyy;
    return public_c;
}