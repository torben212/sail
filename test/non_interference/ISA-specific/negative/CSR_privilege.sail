default Order dec
$include <prelude.sail>

val priv_check : (int, int) -> int

// This function demonstrates that were we to bypass the privilege checks, we might be able to leak information from a higher privilege CSR 
// priv_check is the function which will check whether we are in the correct privilege mode to access the CSR and perform a CSR function
//assuming 0 is the lowest privilege level, we are creating a bypass that shows, how this will break with non-interference

function priv_check (secret_CSR, public_priv) = {
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
    let public_result : int = priv_check(secret_CSR, public_privilege_level);
    return public_result;
}