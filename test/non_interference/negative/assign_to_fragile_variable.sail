default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
  let secret_x : int = 0;
  let public_y : int = 5;
  let fragile_secret_z : int = public_y + secret_x;
  return fragile_secret_z;
}