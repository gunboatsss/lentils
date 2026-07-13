/-
Truncate — IO wrapper for the `truncate` utility.
0BSD

Shrinks or extends files using truncate(2) via C FFI.
For non-existent files, uses open(2)+ftruncate(2) to create them.
-/

import Lentils.Truncate.Logic
import Lentils.Common.Array
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Truncate

open Logic
open Lentils.Common.Array
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/--
Run the `truncate` utility.

Parses arguments, then truncates each file operand to the given size.
When --no-create is not given, creates non-existent files via open+ftruncate.
Requires `-s SIZE` or `-r FILE`.
Returns exit code 0 on success, or non-zero if any truncation fails.
-/
def run (progName : String) (args : List String) : IO UInt32 := do
  let opts := parseArgs args
  if opts.size.isNone && opts.reference.isNone then
    return ← exitUsage progName "you must specify either '--size' or '--reference'"
  if opts.files.isEmpty then
    return ← exitUsage progName "you must specify either '--size' or '--reference'"
  let mut size : UInt64 := 0
  match opts.size with
  | some s => size := s
  | none =>
    match opts.reference with
    | some ref =>
      match ← try some <$> lstatAll ref catch _ => pure none with
      | none =>
        return ← exitError progName (some ref) "cannot stat reference file"
      | some arr =>
        if arr.size ≥ 2 then
          size := arrGet arr 1
        else
          return 1
    | none =>
      return ← exitUsage progName "missing file operand"
  let mut failed := false
  for file in opts.files do
    if opts.noCreate then
      let ex : Bool ← try (System.FilePath.mk file).pathExists catch _ => pure false
      if !ex then
        continue
      -- File exists, use regular truncate
      try
        truncate file size
      catch e =>
        IO.eprintln s!"{progName}: cannot truncate '{file}': {e.toString}"
        failed := true
    else
      -- Try regular truncate first; if it fails with ENOENT, use open+ftruncate
      try
        truncate file size
      catch _ =>
        try
          truncateFile file size
        catch e =>
          IO.eprintln s!"{progName}: cannot truncate '{file}': {e.toString}"
          failed := true
  if failed then
    return 1
  else
    return 0

end Lentils.Truncate
