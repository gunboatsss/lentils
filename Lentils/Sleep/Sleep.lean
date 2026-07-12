/-
Sleep — IO wrapper for the `sleep` utility.
0BSD

Suspends execution for a specified number of seconds.
Uses Sleep.Logic (pure, verified) for argument parsing.
Uses Lean native IO.sleep (millisecond precision).
-/

import Lentils.Sleep.Logic
import Lentils.Common.Errors

namespace Lentils.Sleep

open Logic
open Lentils.Common.Errors

/--
Run the `sleep` utility with the given arguments.
Parses the duration and calls IO.sleep (in milliseconds).
Returns exit code 0 on success, non-zero on error.
-/
def run (args : List String) : IO UInt32 := do
  match args with
  | [] =>
    exitError "sleep" none "missing operand"
  | _ =>
    let mut exitCode : UInt32 := 0
    for arg in args do
      match parseDuration arg with
      | none =>
        IO.eprintln s!"sleep: invalid time interval '{arg}'"
        exitCode := 1
      | some (secs, _nanos) =>
        let totalMs : UInt32 := ((secs.toUInt64 * 1000) + (_nanos.toUInt64 / 1000000)).toUInt32
        try
          IO.sleep totalMs
        catch _ =>
          IO.eprintln "sleep: sleep failed"
          exitCode := 1
    return exitCode

end Lentils.Sleep
