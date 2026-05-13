default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
  let supervisor_x : int = 7;
  if (supervisor_x > 5) then {
    let user_a : int = 10;
    let machine_y : int = 5;
    return user_a + supervisor_x;
  } else {
    return 0;
  }
  
}
