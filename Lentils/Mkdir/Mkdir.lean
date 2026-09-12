/-
Mkdir — IO wrapper for the `mkdir` utility.
0BSD

Creates directories. The `-p`/`--parents` flag creates parent directories
as needed (handled transparently by `IO.FS.createDirAll`).
-/

import Lentils.Mkdir.Logic
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Mkdir

open Logic
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/--
Run the `mkdir` utility.

Parses arguments (handling `-p`/`--parents`), then creates each operand
directory. With `-p`, parent directories are created as needed and existing
directories are not an error. Without `-p`, `File exists` is reported for
existing directories and parents are not created. Returns exit code 0 on
success, or a non-zero code if any directory could not be created.
-/
def run (args : List String) : IO UInt32 := do
  let (opts, paths) := parseArgs args
  if paths.isEmpty then
    IO.eprintln "mkdir: missing operand"
    IO.eprintln "Try 'mkdir --help' for more information."
    return 1
  let mut failed := false
  for path in paths do
    let fp := System.FilePath.mk path
    if opts.parents then
      try
        IO.FS.createDirAll fp
      catch e =>
        IO.eprintln s!"mkdir: cannot create directory '{path}': {formatIoError e.toString}"
        failed := true
    else
      try
        mkdir' path 0o777
      catch e =>
        IO.eprintln s!"mkdir: cannot create directory '{path}': {formatIoError e.toString}"
        failed := true
  if failed then
    return 1
  return 0

end Lentils.Mkdir
