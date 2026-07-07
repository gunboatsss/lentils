/-
Echo — IO wrapper for the `echo` utility.
0BSD

Writes its arguments to stdout, separated by spaces, followed by newline.
Uses Echo.Logic (pure, verified) for output formatting.
IO/FFI side effects are confined to this module.
-/

import Lentils.Echo.Logic

namespace Lentils.Echo

open Logic

/--
Run the `echo` utility with the given arguments.
Writes arguments to stdout:
  - If no arguments, writes just a newline.
  - Otherwise, joins arguments with spaces and appends a newline.
Returns exit code 0 on success, 1 on write error.
-/
def run (args : List String) : IO UInt32 := do
  let output := format args
  try
    IO.print output
    return 0
  catch _ =>
    return 1

end Lentils.Echo
