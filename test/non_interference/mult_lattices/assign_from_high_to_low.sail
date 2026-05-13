default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
  let supervisor_x : int = 0;
  let machine_y : int = 5;
  let user_z = let user_a : int = 10 in 
  user_a + supervisor_x + machine_y; 
  return user_z;
}
