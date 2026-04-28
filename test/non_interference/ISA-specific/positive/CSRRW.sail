default Order dec
$include <prelude.sail>

val foo : (int, int, int) -> int

// Demonstrating the case that if we pass the security requirements for the CSR we are trying to access, we are free to read (and write) to that CSR
//without having breaking non-interference

function foo (public_rd, public_CSR, public_rs1) = {
    var public_rd2 : int = public_rd;
    var public_CSR2 : int = public_CSR;
    public_rd2 = public_CSR2;
    public_CSR2 = public_rs1;
    return public_rd2;
}

val main : unit -> int

function main () = {
    let public_rs1 : int = 5;
    let public_rd : int = 0;
    let public_CSR : int = 10;
    let public_result : int = foo(public_rd, public_CSR, public_rs1);
    return public_result;
}