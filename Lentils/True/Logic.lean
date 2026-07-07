/-
True.Logic — Pure exit-code logic for `true`.
0BSD

This file contains ONLY pure functions — no IO, no FFI.
Formal proofs are at the bottom.
No `sorry` or `admit` allowed.

The `true` utility is the trivial program:
  - Input: none
  - Output: exit code 0
  - Specification: always returns 0

Provenance: POSIX.1-2017, Section "true — return true value".
No GPL source was consulted.
-/

namespace Lentils.True.Logic

/--
The exit code of `true`.  Always 0.
This is the pure specification: `true` succeeds unconditionally.
-/
def exitCode : UInt32 := 0

/--
The exit code is always zero.
This is the formal specification for `true`: success is invariant.
-/
theorem exitCode_is_zero : exitCode = 0 := rfl

/--
Running `true` multiple times yields the same result (idempotence).
-/
example : exitCode = exitCode := rfl

end Lentils.True.Logic
