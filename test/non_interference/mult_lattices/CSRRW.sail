default Order dec
$include <prelude.sail>

val CSRRW : (int, int, int) -> int

// Demonstrating the case that if we were to have access to a higher privelege CSR in a lower privelege mode, we could leak information from the CSR to a public reg
// This is primarily to show, that if we were to get access to this function through check_CSR function, we would be able to leak information.

function CSRRW (supervisor_rd, machine_CSR, user_rs1) = {
    var supervisor_rd2 : int = supervisor_rd;
    var machine_CSR2 : int = machine_CSR;
    supervisor_rd2 = machine_CSR2;
    machine_CSR2 = user_rs1;
    return supervisor_rd2;
}

val main : unit -> int

function main () = {
    let user_rs1 : int = 5;
    let supervisor_rd : int = 0;
    let machine_CSR : int = 10;
    let supervisor_result : int = CSRRW(supervisor_rd, machine_CSR, user_rs1);
    return supervisor_result;
}