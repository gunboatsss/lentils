/-
True — IO wrapper for the `true` utility.
0BSD

Trivial wrapper: always returns exit code 0 regardless of arguments.
Uses True.Logic (pure, verified) for the exit code.
IO/FFI side effects are confined to this module.
-/

import Lentils.True.Logic

namespace Lentils.True

/--
Run the `true` utility with the given arguments.
Always succeeds and returns exit code 0.
Arguments are ignored (POSIX: true ignores all operands).
-/
def run (_args : List String) : IO UInt32 :=
  return Logic.exitCode

end Lentils.True
