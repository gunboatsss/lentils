/-
Unlink — IO wrapper for the `unlink` utility.
0BSD

Calls `unlink(2)` on each operand file via the C FFI.
Prints errors for each failed unlink but continues with remaining files.
Returns exit code 0 if all succeeded, 1 if any failed.

Provenance: POSIX.1-2017, Section "unlink — remove a directory entry".
No GPL source was consulted.
-/

import Lentils.Unlink.Logic
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Unlink

open Logic
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/--
Run the `unlink` utility.

Parses arguments, then removes each file via `unlink(2)`. Errors are reported
to stderr but do not halt processing of remaining files. Returns exit code 0
on success, or 1 if any unlink fails.
-/
def run (args : List String) : IO UInt32 := do
  let (_opts, files) := parseArgs args
  if files.isEmpty then
    return ← exitUsage "unlink" "missing operand"
  let mut failed := false
  for f in files do
    try
      unlink f
    catch e =>
      IO.eprintln s!"unlink: cannot unlink '{f}': {e.toString}"
      failed := true
  return if failed then 1 else 0

end Lentils.Unlink
