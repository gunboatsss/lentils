/-
Arch.Logic — Pure architecture detection for `arch`.
0BSD

The `arch` utility prints the machine architecture name.
On Linux this is read from /proc/sys/kernel/arch.
Pure specification: architecture is a short lowercase string.
-/

namespace Lentils.Arch.Logic

/--
The machine architecture string. Expected values:
  "x86_64", "aarch64", "i686", "armv7l", "riscv64", etc.
-/
def arch : String :=
  "unknown"

/--
Validate that an architecture string is non-empty and reasonable.
-/
def isValid (arch : String) : Bool :=
  arch ≠ "" && arch ≠ "unknown"

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- x86_64 is a valid architecture. -/
example : isValid "x86_64" = true := by
  native_decide

/-- Empty string is not a valid architecture. -/
example : isValid "" = false := by
  native_decide

end Lentils.Arch.Logic


