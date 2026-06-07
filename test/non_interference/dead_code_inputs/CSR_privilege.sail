default Order dec
$include <prelude.sail>

val priv_check : (int, string) -> int

// This function demonstrates that if we have the correct privilege level, then the CSR is public and we can access it
// priv_check is the function which will check whether we are in the correct privilege mode to access the CSR and perform a CSR function, in this case, writing the CSR
//assuming 0 is the lowest privilege level, we are creating a bypass that shows, how this will break with non-interference

function priv_check (supervisor_CSR, user_privilege_level) = {
    if (user_privilege_level == "Supervisor") then {
        return supervisor_CSR;
    }
    else{return 0;}
}

val main : unit -> int

function main () = {
    let user_privilege_level = "Supervisor";
    let supervisor_CSR : int = 10;
    let user_result : int = priv_check(supervisor_CSR, user_privilege_level);
    return user_result;
}