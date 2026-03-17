default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
  let secret_y : int = 0;
  let secret_x : int = 5;
  let public_z : int = let public_a : int = 10 in 
  public_a + secret_x + secret_y; 
  return public_z;
}