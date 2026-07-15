/-
Link — IO wrapper for the `link` utility.
0BSD

Calls `link(2)` on two operands (source and destination) via the C FFI.
Returns exit code 0 on success, 1 on failure.

Provenance: POSIX.1-2017, Section "link — call the link() function".
No GPL source was consulted.
-/

import Lentils.Link.Logic
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Link

open Logic
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/--
Run the `link` utility.

Parses arguments, then creates a hard link from source to destination via
`link(2)`. Prints an error message on failure.
-/
def run (args : List String) : IO UInt32 := do
  let (_opts, operands) := parseArgs args
  if operands.length < 2 then
    return ← exitUsage "link" "missing operand"
  else if operands.length > 2 then
    return ← exitUsage "link" "extra operand"
  let old := operands[0]!
  let new_ := operands[1]!
  try
    link old new_
    return 0
  catch e =>
    IO.eprintln s!"link: cannot create link '{new_}' to '{old}': {e.toString}"
    return 1

end Lentils.Link
