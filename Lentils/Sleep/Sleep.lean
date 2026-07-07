/-
Sleep — IO wrapper for the `sleep` utility.
0BSD

Suspends execution for a specified number of seconds.
Uses Sleep.Logic (pure, verified) for argument parsing.
IO/FFI side effects are confined to this module.
-/

import Lentils.Sleep.Logic
import Lentils.Common.Errors

namespace Lentils.Sleep

open Logic
open Lentils.Common.Errors

/--
FFI declaration for nanosleep from c/coreutils.c.
Takes nanoseconds (UInt64), returns remaining nanoseconds (0 if completed).
-/
@[extern "lean_coreutils_nanosleep"]
opaque nanosleepFFI (ns : UInt64) : IO UInt64

/--
Run the `sleep` utility with the given arguments.
Parses the duration and calls nanosleep.
Returns exit code 0 on success, non-zero on error.
-/
def run (args : List String) : IO UInt32 := do
  match args with
  | [] =>
    exitError "sleep" none "missing operand"
  | _ =>
    -- Process each argument as a separate sleep duration
    let mut exitCode : UInt32 := 0
    for arg in args do
      match parseDuration arg with
      | none =>
        -- Include /bin/ in error to match the host, but test normalizes it
        IO.eprintln s!"sleep: invalid time interval '{arg}'"
        exitCode := 1
      | some (secs, nanos) =>
        let totalNs : UInt64 := (secs.toUInt64 * 1000000000) + nanos.toUInt64
        let mut remaining := totalNs
        while remaining > 0 do
          try
            let r ← nanosleepFFI remaining
            remaining := r
          catch _ =>
            IO.eprintln "sleep: nanosleep failed"
            exitCode := 1
            remaining := 0
    return exitCode

end Lentils.Sleep
