/-
False — IO wrapper for the `false` utility.
0BSD

Trivial wrapper: always returns exit code 1 regardless of arguments.
Uses False.Logic (pure, verified) for the exit code.
IO/FFI side effects are confined to this module.
-/

import Lentils.False.Logic

namespace Lentils.False

/--
Run the `false` utility with the given arguments.
Always fails and returns exit code 1.
Arguments are ignored (POSIX: false ignores all operands).
-/
def run (_args : List String) : IO UInt32 :=
  return Logic.exitCode

end Lentils.False
