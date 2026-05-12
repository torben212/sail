default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
  var secret_x : int = 0;
    while (secret_x > 1 ) {
        let secret_a : int = 3;
        secret_x = secret_x + 1;
    };
  return 1;
}