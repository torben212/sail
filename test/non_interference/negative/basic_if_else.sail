default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
  let secret_x : int = 10;
  if (secret_x > 10) then {let public_y : int = 0} else {let public_y : int = 1};
  return secret_x;
}