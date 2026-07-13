/-
Chmod — IO wrapper for the `chmod` utility.
0BSD

Changes file mode bits using the C FFI call `chmod(2)`. Octal modes are
applied directly; symbolic (ug+-=) modes are computed relative to the current
mode obtained via the `stat(2)` FFI wrapper `statMode`. With `-R`/`--recursive`
directories are traversed and each entry's mode is changed.
-/

import Lentils.chmod.Logic
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.chmod

open Logic
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/--
Compute the final mode for `file` from `modeStr` and apply it via `chmod(2)`.

Returns `true` on success, `false` on failure.
-/
def applyMode (opts : Options) (file modeStr : String) : IO Bool := do
  let finalMode : UInt32 ←
    if isOctal modeStr then
      match parseOctal modeStr with
      | some m => pure m
      | none => IO.eprintln s!"chmod: invalid mode '{modeStr}'"; return false
    else
      let cur : UInt32 ←
        try
          statMode file
        catch e =>
          IO.eprintln s!"chmod: cannot access '{file}': {e.toString}"
          return false
      match computeMode modeStr cur with
      | some m => pure m
      | none => IO.eprintln s!"chmod: invalid mode '{modeStr}'"; return false
  try
    chmod file finalMode
  catch e =>
    IO.eprintln s!"chmod: changing permissions of '{file}': {e.toString}"
    return false
  if opts.verbose then
    IO.println s!"mode of '{file}' changed to {finalMode}"
  return true

/--
Apply a mode to `file`, recursing into directories when `opts.recursive` is
set. Symbolic links are not descended into. Returns `true` on success.
-/
partial def applyRecursive (opts : Options) (file modeStr : String) : IO Bool := do
  let md? ← try some <$> (System.FilePath.mk file).metadata catch _ => pure none
  let isSymlink := md?.map (λ m => m.type == .symlink) |>.getD false
  let isDir := md?.map (λ m => m.type == .dir) |>.getD false
  let ok1 : Bool ← applyMode opts file modeStr
  if opts.recursive && isDir && !isSymlink then
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
    return ← exitUsage "chmod" "[-Rcfv] MODE FILE..."
  let mut failed := false
  for f in files do
    let ok ← if opts.recursive then applyRecursive opts f modeStr else applyMode opts f modeStr
    if !ok then failed := true
  if failed then
    return 1
  else
    return 0

end Lentils.chmod
