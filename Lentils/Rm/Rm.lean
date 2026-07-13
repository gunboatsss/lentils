/-
Rm — IO wrapper for the `rm` utility.
0BSD

Removes files and directories. Files and (with `-d`) empty directories are
removed via the C FFI calls `unlink(2)` and `rmdir(2)`. With `-r`/`--recursive`
directories are removed recursively, descending with `unlink`/`rmdir`.
-/

import Lentils.Rm.Logic
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Rm

open Logic
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/--
Recursively remove a path using C FFI `unlink`/`rmdir`.

Directories are enumerated and their entries removed first, then the
(directory) itself is removed with `rmdir`. Regular files are removed with
`unlink`. Returns `true` on success, `false` on failure.
-/
partial def removeRecursive (path : System.FilePath) : IO Bool := do
  let isDir : Bool ← try path.isDir catch _ => pure false
  if isDir then
    match ← try some <$> path.readDir catch _ => pure none with
    | none =>
        IO.eprintln s!"rm: cannot remove '{path.toString}': Permission denied"
        return false
    | some entries =>
        let mut ok := true
        for e in entries do
          if e.fileName == "." || e.fileName == ".." then
            continue
          if !(← removeRecursive (path / e.fileName)) then
            ok := false
        if ok then
          try rmdir path.toString
          catch e =>
            IO.eprintln s!"rm: cannot remove '{path.toString}': {e.toString}"
            return false
        return ok
  else
    try unlink path.toString
    catch e =>
      IO.eprintln s!"rm: cannot remove '{path.toString}': {e.toString}"
      return false
    return true

/--
Run the `rm` utility.

Parses arguments (handling `-f`, `-i`, `-r`/`--recursive`, `-d`/`--dir`,
`-v`/`--verbose`). Removes each operand. With `-r` directories are removed
recursively. With `-d` empty directories may be removed (via `rmdir`). Returns
exit code 0 on success, or a non-zero code if any removal fails.
-/
def run (args : List String) : IO UInt32 := do
  let (opts, files) := parseArgs args
  if files.isEmpty then
    return ← exitUsage "rm" "[-dfirv] FILE..."
  let mut failed := false
  for f in files do
    let path := System.FilePath.mk f
    let ex : Bool ← try path.pathExists catch _ => pure false
    if !ex then
      if opts.force then
        continue
      else
        IO.eprintln s!"rm: cannot remove '{f}': No such file or directory"
        failed := true
        continue
    -- `ok` tracks this file's own removal result so verbose output is emitted
    -- for every successfully removed file, independent of earlier failures.
    let mut ok := false
    if opts.recursive then
      if ← removeRecursive path then
        ok := true
      else
        failed := true
    else
      let isd : Bool ← try path.isDir catch _ => pure false
      if isd then
        if opts.dir then
          try
            rmdir path.toString
            ok := true
          catch e =>
            IO.eprintln s!"rm: cannot remove '{f}': {e.toString}"
            failed := true
        else
          IO.eprintln s!"rm: cannot remove '{f}': Is a directory"
          failed := true
      else
        try
          unlink path.toString
          ok := true
        catch e =>
          IO.eprintln s!"rm: cannot remove '{f}': {e.toString}"
          failed := true
    if opts.verbose && ok then
      IO.println s!"removed '{f}'"
  if failed then
    return 1
  else
    return 0

end Lentils.Rm
