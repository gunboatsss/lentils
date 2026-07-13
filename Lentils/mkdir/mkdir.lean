/-
Mkdir — IO wrapper for the `mkdir` utility.
0BSD

Creates directories. The `-p`/`--parents` flag creates parent directories
as needed (handled transparently by `IO.FS.createDirAll`).
-/

import Lentils.mkdir.Logic
import Lentils.Common.Errors

namespace Lentils.mkdir

open Logic
open Lentils.Common.Errors

/--
Run the `mkdir` utility.

Parses arguments (handling `-p`/`--parents`), then creates each operand
directory using `IO.FS.createDirAll`. Returns exit code 0 on success,
or a non-zero code if any directory could not be created.
-/
def run (args : List String) : IO UInt32 := do
  let (parents, paths) := parseArgs args
  -- `parents` is accepted for POSIX compatibility; createDirAll already
  -- creates missing parent directories, so it is honoured implicitly.
  let _ := parents
  if paths.isEmpty then
    return ← exitUsage "mkdir" "[-p] DIRECTORY..."
  let mut failed := false
  for path in paths do
    try
      IO.FS.createDirAll (System.FilePath.mk path)
    catch e =>
      IO.eprintln s!"mkdir: cannot create directory '{path}': {e.toString}"
      failed := true
  if failed then
    return 1
  return 0

end Lentils.mkdir
