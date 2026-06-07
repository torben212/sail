default Order dec
$include <prelude.sail>

val priv_check : (int, string) -> int

$[complete]
function priv_check (supervisor_CSR, user_privilege_level) = {
    {
        return(0)
    }
}

val main : unit -> int

$[complete]
function main () = {
    let user_privilege_level = "Supervisor";
    let supervisor_CSR : int = 10;
    let user_result : int = priv_check(supervisor_CSR, user_privilege_level);
    return(user_result)
}

