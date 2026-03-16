default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
  let public_x : int = 0;
  let public_y : int = 5;
  let secret_z : int = public_x + public_y;
  return secret_z;
}