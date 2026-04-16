default Order dec
$include <prelude.sail>

val foo : (int, int, int) -> int

// Demonstrating the case that if we were to have access to a higher privelege CSR in a lower privelege mode, we could leak information from the CSR to a public reg
// This is primarily to show, that if we were to get access to this function through check_CSR function, we would be able to leak information.
function foo (public_rd, secret_CSR, public_rs1) = {
    var public_rd2 : int = public_rd;
    var secret_CSR2 : int = secret_CSR;
    public_rd2 = secret_CSR2;
    secret_CSR2 = public_rs1;
    return public_rd2;
}

val main : unit -> int

function main () = {
    let public_rs1 : int = 5;
    let public_rd : int = 0;
    let secret_CSR : int = 10;
    let public_result : int = foo(public_rd, secret_CSR, public_rs1);
    return public_result;
}