default Order dec
$include <prelude.sail>

val foo : (int, int) -> int

// This function demonstrates that if we have the correct privilege level, then the CSR is public and we can access it
// foo is the function which will check whether we are in the correct privilege mode to access the CSR and perform a CSR function, in this case, writing the CSR
//assuming 0 is the lowest privilege level, we are creating a bypass that shows, how this will break with non-interference

function foo (public_CSR, public_priv) = {
    if (public_priv != 0) then {
        return public_CSR;
    }
    else{return 0;}
}

val main : unit -> int

function main () = {
    let public_privilege_level : int = 1;
    let public_CSR : int = 10;
    let public_result : int = foo(public_CSR, public_privilege_level);
    return public_result;
}