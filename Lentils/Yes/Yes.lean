/-
Yes — IO wrapper for the `yes` utility.
0BSD

Repeatedly outputs the message string until killed.
Uses Yes.Logic (pure, verified) for the message string.
IO/FFI side effects are confined to this module.
-/

import Lentils.Yes.Logic

namespace Lentils.Yes

open Logic

/--
Run the `yes` utility with the given arguments.
Repeatedly prints the message (default "y") to stdout.
Continues until stdout is closed (SIGPIPE) or the process is killed.
Returns exit code 0 when terminated via SIGPIPE.
-/
partial def run (args : List String) : IO UInt32 := do
  let msg := message args
  let rec loop : IO UInt32 := do
    try
      IO.println msg
      loop
    catch _ =>
      return exitCode
  loop

end Lentils.Yes
