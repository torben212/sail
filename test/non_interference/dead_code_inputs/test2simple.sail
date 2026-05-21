default Order dec
$include <prelude.sail>

val trapcause_is_interrupt : int -> bool

function trapcause_is_interrupt (user_cause)  = {
    if (user_cause == 0) then {return true;} else {return false;};
}

function trapCause_bits (user_c : int) -> int = {
    return 1;
}

function zicfilp_preserve_elp_on_trap(user_del_priv : string) -> unit = {
  if (user_del_priv == "Machine") then {
    let machine_mstatus_mpelp = user_del_priv;
  } else {
    let supervisor_mstatus_spelp = user_del_priv;
  }
}

function bool_to_bit (user_b : bool) -> int = {
  if user_b then { return 1; } else { return 0; }
}

function zero_extend (user_i : int) -> int = {
  return 1;
}

function trap_handler(user_del_priv : string, user_c : int, user_pc : int)-> int = {
  let machine_mstatus_mie = 0;
  let supervisor_mstatus_pie = 0;

  let user_is_interrupt = trapcause_is_interrupt(user_c);
  let user_cause        = trapCause_bits(user_c);

  zicfilp_preserve_elp_on_trap(user_del_priv);

  if (user_del_priv == "Machine") then {
    let machine_mcause_is_interrupt = bool_to_bit(user_is_interrupt);
    let machine_mcause_cause        = zero_extend(user_cause);
    let machine_mstatus_mpie        = machine_mstatus_mie;
    let machine_mstatus_mie         = 0;
    let machine_mepc                 = user_pc;
    return 0;
  } else if (user_del_priv == "Supervisor") then {
    let supervisor_scause_is_interrupt = bool_to_bit(user_is_interrupt);
    let supervisor_scause_cause       = zero_extend(user_cause);
    let supervisor_mstatus_spie         = supervisor_mstatus_pie;
    let supervisor_mstatus_sie          = 0;
    let supervisor_sepc                = user_pc;
    return 0;
  } else {
    return 0;
  }
}

function main() -> unit = {
  let user_del_priv = "Machine";
  let user_c = 1;
  let user_pc = 1;
  let user_result = trap_handler(user_del_priv, user_c, user_pc);
}