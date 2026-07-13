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

end Lentils.Arch.Logic
