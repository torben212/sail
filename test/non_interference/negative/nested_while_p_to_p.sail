default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
  let secret_x : int = 0;
  let public_a : int = 0;
    while (secret_x > 1 ) {
        while (public_a > 1) {
            let secret_y : int = secret_x;
        };
    };
  return 1;
}