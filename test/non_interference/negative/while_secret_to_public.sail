default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
  let secret_x : int = 0;
    while (secret_x > 1 ) {
        let public_a = secret_x;
    };
  return 1;
}