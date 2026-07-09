/-
Pwd — IO wrapper for the `pwd` utility.
0BSD

Uses Lean's native `IO.currentDir` for getcwd.
Uses Pwd.Logic (pure, verified) for output formatting.
-/

import Lentils.Pwd.Logic
import Lentils.Common.Errors

namespace Lentils.Pwd

open Logic
open Lentils.Common.Errors

/--
Run the `pwd` utility.
Gets the current working directory and prints it to stdout.
Returns exit code 0 on success, 1 on error.
-/
def run (_args : List String) : IO UInt32 := do
  try
    let cwd ← IO.currentDir
    let output := format cwd.toString
    IO.print output
    return (0 : UInt32)
  catch _ =>
    exitError "pwd" none "cannot get current directory"

end Lentils.Pwd
