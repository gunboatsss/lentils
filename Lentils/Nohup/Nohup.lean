/-
Lentils.Nohup — IO wrapper for the `nohup` utility.
0BSD

Runs a command immune to SIGHUP (hangup) signals.
Uses Nohup.Logic (pure, verified) for argument parsing.
Uses fork/exec FFI (lean_coreutils_run_nohup) that ignores SIGHUP in
the child and redirects standard streams away from the terminal.
-/

import Lentils.Nohup.Logic
import Lentils.Common.Errors

namespace Lentils.Nohup

open Logic
open Lentils.Common.Errors

/-- FFI: run a command in a forked child that ignores SIGHUP. -/
@[extern "lean_coreutils_run_nohup"]
opaque runNohup (cmdArgs : Array String) : IO UInt32

/--
Run the `nohup` utility with the given arguments.
Parses the command, then execs via FFI.
Returns the command's exit code, or an error code on failure.
-/
def run (args : List String) : IO UInt32 := do
  match parseArgs args with
  | none =>
    IO.eprintln "nohup: missing operand"
    return 1
  | some cmd =>
    if cmd.isEmpty then
      IO.eprintln "nohup: missing operand"
      return 1
    else
      try
        let code ← runNohup cmd.toArray
        return code
      catch _ =>
        IO.eprintln "nohup: failed to execute command"
        return 127

end Lentils.Nohup
