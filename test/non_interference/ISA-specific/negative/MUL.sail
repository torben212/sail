default Order dec
$include <prelude.sail>

val MUL : (int, int, int) -> int

function MUL (secret_rs1, secret_rs2, public_rd) = {
  var public_rd2 : int = public_rd;
  public_rd2 = secret_rs1 * secret_rs2;
  return public_rd2
}

val main : unit -> int

function main () = {
  let secret_rs1 : int = 5;
  let secret_rs2 : int = 10;
  let public_rs3 : int = 0;
  let public_result = MUL(secret_rs1, secret_rs2, public_rs3);
  return public_result;
}