/-
Chgrp — IO wrapper for the `chgrp` utility.
0BSD

Changes group ownership of files via the `chown(2)` C FFI (with empty
owner string to change group only).

Provenance: POSIX.1-2017, Section "chgrp — change group ownership".
No GPL source was consulted.
-/

import Lentils.Chgrp.Logic
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Chgrp

open Logic
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/--
Run the `chgrp` utility.

Parses `[OPTIONS] GROUP FILE...` and calls `chown(2)` (empty owner) on each file.
-/
def run (args : List String) : IO UInt32 := do
  let parsed := parseArgs args
  if parsed.group.isEmpty then
    return ← exitUsage "chgrp" "missing operand"
  if parsed.files.isEmpty then
    return ← exitUsage "chgrp" s!"missing operand after '{parsed.group}'"
  let mut failed := false
  for f in parsed.files do
    try
      chown f "" parsed.group
      if parsed.options.verbose then
        IO.println s!"changed group of '{f}' to '{parsed.group}'"
    catch e =>
      IO.eprintln s!"chgrp: changing group of '{f}': {e.toString}"
      failed := true
  return if failed then 1 else 0

end Lentils.Chgrp
