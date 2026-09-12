/-
Echo — IO wrapper for the `echo` utility.
0BSD

Writes arguments to stdout, separated by spaces, followed by newline.
Uses Echo.Logic (pure, verified) for output formatting.
IO/FFI side effects are confined to this module.
-/

import Lentils.Echo.Logic

namespace Lentils.Echo

open Logic

/--
Run the `echo` utility with the given arguments.
Parses flags from args and uses the verified Logic.format.
Returns exit code 0 on success, 1 on write error.
-/
def run (args : List String) : IO UInt32 := do
  -- Parse -n flag: if the first arg is "-n", set suppressNewline
  -- (the Logic layer handles consuming multiple -n flags)
  let suppressNewline := args.head? == some "-n"
  let input : EchoInput := { suppressNewline := false, args := args }
  let output := format input
  try
    IO.print output
    return 0
  catch _ =>
    return 1

end Lentils.Echo
