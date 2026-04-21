default Order dec
$include <prelude.sail>

val main : (int, int) -> int

function main (public_z, public_b) = {
  if (public_z > 10) then {
    let secret_b = 1;
  }
  else {
    let secret_b = 2;
  };
  return 1;
}