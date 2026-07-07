/-
False.Logic — Pure exit-code logic for `false`.
0BSD

This file contains ONLY pure functions — no IO, no FFI.
Formal proofs are at the bottom.
No `sorry` or `admit` allowed.

The `false` utility is the trivial program:
  - Input: none
  - Output: exit code 1
  - Specification: always returns non-zero (typically 1)

Provenance: POSIX.1-2017, Section "false — return false value".
No GPL source was consulted.
-/

namespace Lentils.False.Logic

/--
The exit code of `false`.  Always 1.
This is the pure specification: `false` always indicates failure.
-/
def exitCode : UInt32 := 1

/--
The exit code is non-zero.
This is the formal specification for `false`: failure is invariant.
-/
theorem exitCode_is_nonzero : exitCode ≠ 0 := by
  decide

/--
Running `false` multiple times yields the same result (idempotence).
-/
example : exitCode = exitCode := rfl

end Lentils.False.Logic
