/-
Touch — IO wrapper for the `touch` utility.
0BSD

Updates the access and modification times of files, creating them if they
do not exist (unless `-c`/`--no-create` is given). Uses `IO.FS.Handle.mk`
in append mode, which creates the file when missing and leaves existing
content untouched, then relies on handle GC to close it. With `-c` the file
is only opened when it already exists.
-/

import Lentils.Touch.Logic
import Lentils.Common.Errors

namespace Lentils.Touch

open Logic
open Lentils.Common.Errors

/--
Open (and thereby create/refresh) a single file.

Opening in append mode creates the file if it does not exist and does not
truncate an existing file. The handle is dropped after the opener returns,
and is closed by the runtime when collected.
-/
def touchOne (path : System.FilePath) : IO Bool := do
  try
    IO.FS.withFile path IO.FS.Mode.append (fun _ => pure ())
    return true
  catch e =>
    IO.eprintln s!"touch: cannot touch '{path.toString}': {e.toString}"
    return false

/--
Run the `touch` utility.

Parses arguments, then touches each file operand. With `-c`/`--no-create`,
a file is only touched if it already exists. Returns exit code 0 on success,
or a non-zero code if any file could not be touched.
-/
def run (args : List String) : IO UInt32 := do
  let (opts, files) := parseArgs args
  if files.isEmpty then
    return ← exitUsage "touch" "[-cf] FILE..."
  let mut failed := false
  for file in files do
    let path := System.FilePath.mk file
    if opts.noCreate then
      let ex : Bool ← try path.pathExists catch _ => pure false
      if !ex then
        continue
    if !(← touchOne path) then
      failed := true
  if failed then
    return 1
  else
    return 0

end Lentils.Touch
