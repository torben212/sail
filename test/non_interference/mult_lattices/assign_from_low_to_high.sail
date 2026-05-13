default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
  let user_x : int = 0;
  let supervisor_y : int = 5;
  let machine_z = let user_a : int = 10 in 
  user_a + user_x + supervisor_y; 
  return machine_z;
}
