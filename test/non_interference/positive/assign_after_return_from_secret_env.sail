default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
  let secret_x : int = 10;
  let public_z : int = 1;
  if (secret_x > 10) then {
    if (public_z > 1) then {let secret_y = secret_x};
    };
  let public_g = 1;
  return secret_x;
}