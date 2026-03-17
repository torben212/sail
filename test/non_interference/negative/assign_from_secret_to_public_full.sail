default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
  let secret_y : int = 0;
  let public_z : int = secret_y;
  return public_z;
}