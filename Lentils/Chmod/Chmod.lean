/-
Chmod — IO wrapper for the `chmod` utility.
0BSD

Changes file mode bits using the C FFI call `chmod(2)`. Octal modes are
applied directly; symbolic (ug+-=) modes are computed relative to the current
mode obtained via the `stat(2)` FFI wrapper `statMode`. With `-R`/`--recursive`
directories are traversed and each entry's mode is changed.
-/

import Lentils.Chmod.Logic
import Lentils.Common.Array
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Chmod

open Logic
open Lentils.Common.Array
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/-- Determine file type nibble from lstat mode bits. -/
def fileType (mode : UInt64) : UInt64 :=
  (mode >>> 12) &&& 0xF

/--
Compute the final mode for `file` from `modeStr` and apply it via `chmod(2)`.

Returns `true` on success, `false` on failure.
-/
def applyMode (opts : Options) (file modeStr : String) : IO Bool := do
  let finalMode : UInt32 ←
    if isOctal modeStr then
      match parseOctal modeStr with
      | some m => pure m
      | none =>
        IO.eprintln s!"chmod: invalid mode: '{modeStr}'"
        IO.eprintln s!"Try 'chmod --help' for more information."
        return false
    else
      let cur : UInt32 ←
        try
          statMode file
        catch e =>
          IO.eprintln s!"chmod: cannot access '{file}': {formatIoError e.toString}"
          return false
      match computeMode modeStr cur with
      | some m => pure m
      | none =>
        IO.eprintln s!"chmod: invalid mode: '{modeStr}'"
        IO.eprintln s!"Try 'chmod --help' for more information."
        return false
  try
    chmod file finalMode
  catch e =>
    IO.eprintln s!"chmod: cannot access '{file}': {formatIoError e.toString}"
    return false
  if opts.verbose then
    IO.println s!"mode of '{file}' changed to {finalMode}"
  return true

/--
Apply a mode to `file`, recursing into directories when `opts.recursive` is
set. Uses lstat so symlinks are detected (not followed). GNU chmod -R ignores
symlinks entirely: does not change their target's mode and does not descend.
Returns `true` on success.
-/
partial def applyRecursive (opts : Options) (file modeStr : String) : IO Bool := do
  let arr? ← try some <$> lstatAll file catch _ => pure none
  match arr? with
  | none =>
    IO.eprintln s!"chmod: cannot access '{file}': No such file or directory"
    return false
  | some arr =>
    let typ := fileType (arrGet arr 0)
    -- GNU chmod -R never follows symlinks: skip entirely
    if typ == 0xA then
      return true
    let isDir := typ == 0x4
    let ok1 : Bool ← applyMode opts file modeStr
    if opts.recursive && isDir then
      match ← try some <$> (System.FilePath.mk file).readDir catch _ => pure none with
      | some entries =>
          let mut ok := ok1
          for e in entries do
            if e.fileName == "." || e.fileName == ".." then
              continue
            let child := (System.FilePath.mk file / e.fileName).toString
            if !(← applyRecursive opts child modeStr) then
              ok := false
          return ok
      | none => return ok1
    else
      return ok1

/--
Run the `chmod` utility.

Parses arguments (handling `-R`/`--recursive`, `-v`/`--verbose`, `-f`/`--force`,
`-c`/`--changes`), then applies the mode to each file operand. Returns exit
code 0 on success, or a non-zero code if any change fails.
-/
def run (args : List String) : IO UInt32 := do
  let (opts, modeStr, files) := parseArgs args
  if modeStr.isEmpty || files.isEmpty then
    IO.eprintln "chmod: missing operand"
    IO.eprintln "Try 'chmod --help' for more information."
    return 1
  let mut failed := false
  for f in files do
    let ok ← if opts.recursive then applyRecursive opts f modeStr else applyMode opts f modeStr
    if !ok then failed := true
  if failed then
    return 1
  else
    return 0

end Lentils.Chmod
