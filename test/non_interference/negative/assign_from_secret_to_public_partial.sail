default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
  let secret_x : int = 0;
  let public_y : int = 5;
  let public_z : int = secret_x + public_y;
  return public_z;
}