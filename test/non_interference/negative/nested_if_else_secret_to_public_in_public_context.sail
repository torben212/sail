default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
  let secret_x : int = 10;
  let public_z : int = 1;
  if (public_z < 10) then {
    if (public_z > 1) then {let public_a = secret_x};
    } 
    else {let public_y : int = 1};
  return secret_x;
}