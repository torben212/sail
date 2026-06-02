default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
  let user_x : int = 0;
  if (user_x > 0) then {
    let user_z : int = user_x;
    return user_z;
  } else {
    return 0;
  }
}
