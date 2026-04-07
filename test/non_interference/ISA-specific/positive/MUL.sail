default Order dec
$include <prelude.sail>

val foo : (int, int, int) -> int



function foo (secret_rs1, secret_rs2, secret_rd) = {
    var secret_rd2 : int = secret_rd;
    secret_rd2 = secret_rs1 * secret_rs2;
    return secret_rd2;

}

val main : unit -> int

function main () = {
  let secret_rs1 : int = 5;
  let secret_rs2 : int = 10;
  let secret_rs3 : int = 0;
  let secret_result = foo(secret_rs1, secret_rs2, secret_rs3);
  return secret_result;
}