default Order dec
$include <prelude.sail>

// Assume these functions are defined as part of the ISA's memory model
val read_mem : (bitvector(16), int) -> int
function read_mem (secret_address, public_size_in_bytes) = {
    // This function would read from memory at the given address and return the value
    // For the sake of this test, we can assume it returns a dummy value
    let secret_dummy_value : int = 42; // This represents the secret data at the memory address
    return secret_dummy_value; // Return the secret value
}
val main : unit -> int
// The test function
function main() = {
  var public_c : int = 0;
  let public_base_address : bitvector(16) = 0x1000;
  let secret_offset : bitvector(16) = 0x0004;
  let public_read_size_in_bytes : int = 4;
  let secret_address : bitvector(16) = add_bits(public_base_address, secret_offset);

  // The value of secret_address is used to access memory.
  // An observer could potentially deduce secret_address by observing
  // which cache lines are accessed or if a page fault occurs.
  public_c = read_mem(secret_address, public_read_size_in_bytes);

  return public_c;
}