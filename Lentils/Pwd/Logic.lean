/-
Pwd.Logic — Pure specification for `pwd`.
0BSD

This file contains ONLY pure functions — no IO, no FFI.
Formal proofs are at the bottom.
No `sorry` or `admit` allowed.

The `pwd` utility prints the current working directory.
Since the actual path comes from the OS via getcwd, the pure logic
just specifies the exit code as always 0 when successful.

Provenance: POSIX.1-2017, Section "pwd — return working directory name".
No GPL source was consulted.
-/

namespace Lentils.Pwd.Logic

/--
The exit code of `pwd` on success. Always 0.
pwd always succeeds unless getcwd fails (e.g., unlinked directory).
-/
def exitCode : UInt32 := 0

/--
Format the working directory for output.
Simply appends a newline to the path.
-/
def format (path : String) : String :=
  path ++ "\n"

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/--
The exit code is always zero.
-/
theorem exitCode_is_zero : exitCode = 0 := rfl

/--
Format appends a newline to the given path.
-/
theorem format_append_newline (path : String) : format path = path ++ "\n" := rfl

/--
Format is injective: if format p1 = format p2 then p1 = p2.
-/
theorem format_injective (p1 p2 : String) (h : format p1 = format p2) : p1 = p2 := by
  simpa [format] using h

/--
Idempotence: format produces the same output given the same input.
-/
theorem format_idempotent (path : String) : format path = format path := rfl

end Lentils.Pwd.Logic
