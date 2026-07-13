/-
Lentils.Nice — IO wrapper for the `nice` utility.
0BSD

Runs a command with a modified scheduling priority.
Uses Nice.Logic (pure, verified) for argument parsing.
Uses fork/exec FFI (lean_coreutils_run_nice) that calls nice(2)
in the child before exec.
-/

import Lentils.Nice.Logic
import Lentils.Common.Errors

namespace Lentils.Nice

open Logic
open Lentils.Common.Errors

/-- FFI: run a command in a forked child after nice(adjustment). -/
@[extern "lean_coreutils_run_nice"]
opaque runNice (adjustment : Int32) (cmdArgs : Array String) : IO UInt32

/--
Run the `nice` utility with the given arguments.
Parses the adjustment and command, then execs via FFI.
Returns the command's exit code, or an error code on failure.
-/
def run (args : List String) : IO UInt32 := do
  match parseArgs args with
  | none =>
    IO.eprintln "nice: missing operand"
    return 1
  | some (adj, cmd) =>
    if cmd.isEmpty then
      IO.eprintln "nice: missing operand"
      return 1
    else
      try
        let code ← runNice adj cmd.toArray
        return code
      catch _ =>
        IO.eprintln "nice: failed to execute command"
        return 127

end Lentils.Nice
