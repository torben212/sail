default Order dec
$include <prelude.sail>

val foo : (int, int) -> int

// This test is to demonstrate that we can read from a read only csr.

function foo (fragile_public_CSR, public_operation_access_level) = {
    let public_CSR_access_level : int = 0; //assuming 0 means that it is a read-only CSR and everything above means we have a write or a read_write operation
    if (public_operation_access_level <= public_CSR_access_level) then {
        return fragile_public_CSR;
    }
    else {
        return 0; //we can't write to a read-only csr
    }
}

val main : unit -> int

function main () = {
    let public_operation_access_level : int = 0;
    let fragile_public_CSR : int = 10;
    let public_result : int = foo(fragile_public_CSR, public_operation_access_level);
    return public_result;
}