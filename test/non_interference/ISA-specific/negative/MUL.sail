default Order dec
$include <prelude.sail>

val MUL : (int, int) -> int

function MUL (secret_rs1, secret_rs2) = {
  var public_rd : int = 0;
  public_rd = secret_rs1 * secret_rs2;
  return public_rd
}

val main : unit -> int

function main () = {
  let secret_rs1 : int = 5;
  let secret_rs2 : int = 10;
  let public_result : int = MUL(secret_rs1, secret_rs2);
  return public_result;
}