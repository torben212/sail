default Order dec
$include <prelude.sail>

val foo : (int, int) -> int

// This test is to demonstrate that if we were able to bypass the access control checks, then we could write to a read-only CSR
function foo (fragile_public_CSR, public_operation_access_level) = {
    let public_bypass : bool = true;
    var fragile_public_CSR2 : int = fragile_public_CSR;
    let public_new_CSR : int = 1;
    let public_CSR_access_level : int = 0; //assuming 0 means that it is a read-only CSR and everything above means we have a write or a read_write operation
    if (public_operation_access_level <= public_CSR_access_level | public_bypass) then {
        fragile_public_CSR2 = public_new_CSR; //if we were able to bypass the access control checks, we could write to this read-only CSR
        return fragile_public_CSR2;
    }
    else {
        return fragile_public_CSR2; //if we were able to bypass the access control checks, we could leak information from this CSR
    }
}

val main : unit -> int

function main () = {
    let public_operation_access_level : int = 1;
    let fragile_public_CSR : int = 10;
    let public_result : int = foo(fragile_public_CSR, public_operation_access_level);
    return public_result;
}