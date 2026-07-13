/-
Lentils.Timeout — IO wrapper for the `timeout` utility.
0BSD

Runs a command and kills it if it does not finish within a duration.
Uses Timeout.Logic (pure, verified) for argument parsing.
Uses fork/exec FFI (lean_coreutils_run_timeout) that sets an alarm(2)
timer in the parent and signals the child on expiry.
-/

import Lentils.Timeout.Logic
import Lentils.Common.Errors

namespace Lentils.Timeout

open Logic
open Lentils.Common.Errors

/-- FFI: run a command, sending sig after seconds (and SIGKILL after killAfter). -/
@[extern "lean_coreutils_run_timeout"]
opaque runTimeout (seconds : UInt32) (sig : UInt32) (killAfter : UInt32)
    (cmdArgs : Array String) : IO UInt32

/--
Run the `timeout` utility with the given arguments.
Returns:
  - 124 if the command timed out
  - the command's exit code otherwise
  - 1 or 127 on usage/execution errors
-/
def run (args : List String) : IO UInt32 := do
  match parseArgs args with
  | none =>
    IO.eprintln "timeout: missing operand"
    return 1
  | some cfg =>
    if cfg.cmd.isEmpty then
      IO.eprintln "timeout: missing operand"
      return 1
    else if cfg.seconds.isNone then
      IO.eprintln "timeout: missing duration operand"
      return 1
    else
      try
        let secs := (cfg.seconds.getD 0).toUInt32
        let sigN := cfg.signal.toUInt32
        let killAfter := cfg.killAfter.toUInt32
        let code ← runTimeout secs sigN killAfter cfg.cmd.toArray
        return code
      catch _ =>
        IO.eprintln "timeout: failed to execute command"
        return 127

end Lentils.Timeout
