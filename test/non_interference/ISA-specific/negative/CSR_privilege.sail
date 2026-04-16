default Order dec
$include <prelude.sail>

val foo : (int, int) -> int

// This function demonstrates that were we to bypass the privilege checks, we might be able to leak information from a higher privilege CSR 
// foo is the function which will check whether we are in the correct privilege mode to access the CSR and perform a CSR function
//assuming 0 is the lowest privilege level, we are creating a bypass that shows, how this will break with non-interference
function foo (secret_CSR, public_priv) = {
    let public_bypass : bool = true;
    if (public_priv != 0 | public_bypass) then {
        return secret_CSR;
    }
    else{return 0;}
}

val main : unit -> int

function main () = {
    let public_privilege_level : int = 0;
    let secret_CSR : int = 10;
    let public_result : int = foo(secret_CSR, public_privilege_level);
    return public_result;
}