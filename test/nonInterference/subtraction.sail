default Order dec
$include <prelude.sail>

val main : unit -> int

function main () = {
  let x : int = 0;
  let y : int = 5;
  let z : int = x - y;
  return z;
}