/-
Chown — IO wrapper for the `chown` utility.
0BSD

Changes file owner and/or group via the `chown(2)` C FFI.
Supports the `owner[:group]` syntax.

Provenance: POSIX.1-2017, Section "chown — change file owner and group".
No GPL source was consulted.
-/

import Lentils.Chown.Logic
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Chown

open Logic
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/--
Run the `chown` utility.

Parses `[OPTIONS] OWNER[:GROUP] FILE...` and calls `chown(2)` on each file.
-/
def run (args : List String) : IO UInt32 := do
  let (opts, spec, files) := parseArgs args
  match spec with
  | none => return ← exitUsage "chown" "missing operand"
  | some og =>
    if files.isEmpty then
      return ← exitUsage "chown" s!"missing operand after '{og.owner}:{og.group}'"
    let mut failed := false
    for f in files do
      try
        chown f og.owner og.group
        if opts.verbose then
          IO.println s!"changed ownership of '{f}'"
      catch e =>
        IO.eprintln s!"chown: changing ownership of '{f}': {e.toString}"
        failed := true
    return if failed then 1 else 0

end Lentils.Chown
