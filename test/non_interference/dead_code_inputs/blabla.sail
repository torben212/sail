default Order dec
$include <prelude.sail>

val foo : int -> int

function foo(secret_a) = {
    let secret_b = 1;
    if (secret_b > 0) then {
        let public_c = secret_a; 
    } else if (secret_b == -1) then {
        let public_c = 1;
    } else {
        let public_c = 1;
    };
    return 1;
}

val main : unit -> int

function main () = {
  return 1;
}