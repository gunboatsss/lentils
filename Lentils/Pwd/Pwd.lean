/-
Pwd — IO wrapper for the `pwd` utility.
0BSD

Calls getcwd via FFI and prints the result.
Uses Pwd.Logic (pure, verified) for output formatting.
IO/FFI side effects are confined to this module.
-/

import Lentils.Pwd.Logic
import Lentils.Common.Errors

namespace Lentils.Pwd

open Logic
open Lentils.Common.Errors

/--
FFI declaration for getcwd from c/coreutils.c.
-/
@[extern "lean_coreutils_getcwd"]
opaque getcwdFFI : Unit → IO String

/--
Run the `pwd` utility.
Calls getcwd and prints the working directory to stdout.
Returns exit code 0 on success, 1 on error.
-/
def run (_args : List String) : IO UInt32 := do
  try
    let cwd ← getcwdFFI ()
    let output := format cwd
    IO.print output
    return (0 : UInt32)
  catch _ =>
    exitError "pwd" none "cannot get current directory"

end Lentils.Pwd
