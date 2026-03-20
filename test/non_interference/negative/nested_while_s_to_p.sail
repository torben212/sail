default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
  let secret_x : int = 0;
  let public_a : int = 0;
    while (secret_x > 1 ) {
        while (public_a > 1) {
            let public_b : int = secret_x;
        };
    };
  return public_a;
}