/-
Kill — IO wrapper for the `kill` utility. 0BSD

Sends a signal to a process (default: SIGTERM).
Uses Kill.Logic (pure, verified) for argument parsing.
Uses FFI kill(2) for signal delivery.

Supported command forms:
  kill [-s signal] pid...
  kill -l [exit_status]
  kill -signal pid...
  kill --signal signal pid...
-/

import Lentils.Kill.Logic
import Lentils.Common.Errors

namespace Lentils.Kill

open Logic
open Lentils.Common.Errors

/-- FFI: send a signal to a process. -/
@[extern "lean_coreutils_kill"]
opaque killFfi (pid : Int32) (sig : Int32) : IO Unit

/--
Kill a single pid with the given signal.
Returns 0 on success, prints error and returns 1 on failure.
-/
def killOne (pidStr : String) (signal : Int) : IO UInt32 := do
  match parsePid pidStr with
  | none =>
    IO.eprintln s!"kill: invalid pid '{pidStr}'"
    return 1
  | some pid =>
    try
      killFfi (pid.toInt32) (signal.toInt32)
      return 0
    catch e =>
      IO.eprintln s!"kill: ({pidStr}) - {e.toString}"
      return 1

/--
Parse arguments and attempt to kill the specified processes.
Returns exit code 0 on success, 1 on error.
-/
def run (args : List String) : IO UInt32 := do
  match args with
  | [] =>
    return ← exitUsage "kill" "missing operand"

  | "-l" :: rest =>
    match rest with
    | [] =>
      IO.println formatSignalList
      return 0
    | [exitStatusStr] =>
      match parseInt exitStatusStr with
      | some n =>
        IO.println (formatExitStatus n)
        return 0
      | none =>
        IO.eprintln s!"kill: unknown signal '{exitStatusStr}'"
        return 1
    | _ =>
      IO.eprintln "kill: extra operand after '-l'"
      return 1

  | _ =>
    match parseKillArgs args defaultSignal with
    | Except.error (KillError.invalidSignal name) =>
      IO.eprintln s!"kill: unknown signal '{name}'"
      return 1
    | Except.error (KillError.missingArg opt) =>
      IO.eprintln s!"kill: option requires an argument -- '{opt}'"
      return 1
    | Except.ok (signal, pids) =>
      if List.isEmpty pids then
        return ← exitUsage "kill" "missing operand"
      let results : List UInt32 ← List.mapM (λ p => killOne p signal) pids
      let exitCode := List.foldl (λ (acc : UInt32) (x : UInt32) => max acc x) 0 results
      return exitCode

end Lentils.Kill
