default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
  let secret_x : int = 0;
  let secret_y : int = 5;
  let secret_z : int = secret_x + secret_y;
  return secret_z;
}