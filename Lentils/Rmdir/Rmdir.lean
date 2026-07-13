/-
Rmdir — IO wrapper for the `rmdir` utility.
0BSD

Removes empty directories using the C FFI call `rmdir(2)`. With
`-p`/`--parents` each directory argument is removed along with its now-empty
ancestors.
-/

import Lentils.Rmdir.Logic
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Rmdir

open Logic
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/--
Remove a single empty directory via `rmdir(2)`.
Returns `true` on success, `false` on failure.
-/
def removeOne (p : System.FilePath) : IO Bool := do
  try
    rmdir p.toString
    return true
  catch e =>
    IO.eprintln s!"rmdir: failed to remove '{p.toString}': {e.toString}"
    return false

/--
Remove `p` and, when `parents` is true, its empty ancestors.

Starting from the deepest component, each directory is removed with `rmdir(2)`;
if `parents` is set the process continues with the parent directory until no
parent remains. Returns `true` if every requested removal succeeded.
-/
partial def removeChain (p : System.FilePath) (parents : Bool) : IO Bool := do
  if !(← removeOne p) then
    return false
  if !parents then
    return true
  match p.parent with
  | none => return true
  | some parent =>
      if parent == p then
        return true
      removeChain parent parents

/--
Run the `rmdir` utility.

Parses arguments (handling `-p`/`--parents` and `-v`/`--verbose`), then removes
each operand directory. With `-p` the directory's empty ancestors are removed
as well. Returns exit code 0 on success, or a non-zero code if any removal
fails.
-/
def run (args : List String) : IO UInt32 := do
  let (opts, paths) := parseArgs args
  if paths.isEmpty then
    return ← exitUsage "rmdir" "[-pv] DIRECTORY..."
  let mut failed := false
  for dir in paths do
    let path := System.FilePath.mk dir
    if !(← removeChain path opts.parents) then
      failed := true
    else if opts.verbose then
      IO.println s!"removed directory '{dir}'"
  if failed then
    return 1
  else
    return 0

end Lentils.Rmdir
