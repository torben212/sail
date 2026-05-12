default Order dec
$include <prelude.sail>

val main : unit -> int

val set1 : register(int) -> unit

register public_x : int = 0

function set1 (public_q) = {
    (*public_q) = 1;
    return ();
}

function main () = {
    set1(ref public_x);
    return public_x;
}