default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
  let supervisor_x : int = 0;
  let supervisor_y : int = 5;
  if (supervisor_y > 0) then {
    let user_z : int = supervisor_x;
    return user_z;
  } else {
    return 0;
  }
}