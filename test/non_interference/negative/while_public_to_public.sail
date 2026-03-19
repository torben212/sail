default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
  let secret_x : int = 0;
  let public_a : int = 1;
    while (secret_x > 1 ) {
        let public_b : int = public_a;
    };
  return 1;
}