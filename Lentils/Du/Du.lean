/-
Du — IO wrapper for the `du` utility.
0BSD

Estimates file space usage using lstat(2) (via C FFI), recursing into directories.
Uses lstat to avoid following symlinks (matches GNU du behavior).
-/

import Lentils.Du.Logic
import Lentils.Common.Array
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Du

open Logic
open Lentils.Common.Array
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/--
Recursively compute disk usage for a path using lstat.
Returns (totalBlocks, isDir, ok) where totalBlocks is in 512-byte units.
ok is false if the path could not be accessed.
-/
partial def duPath (progName : String) (path : System.FilePath) (depth : Int) (opts : Options) : IO (UInt64 × Bool × Bool) := do
  match ← try some <$> lstatAll path.toString catch _ => pure none with
  | none =>
    IO.eprintln s!"{progName}: cannot access '{path.toString}': No such file or directory"
    return (0, false, false)
  | some arr =>
    if arr.size < 6 then
      return (0, false, false)
    else
    let mode := arrGet arr 0
    let blocks := arrGet arr 5
    let isDir := ((mode >>> 12) &&& 0xF == 0x4)
    if !isDir then
      return (blocks, false, true)
    else
      let mut total := blocks
      match ← try some <$> path.readDir catch _ => pure none with
      | none =>
        IO.eprintln s!"{progName}: cannot read directory '{path.toString}': Permission denied"
        return (total, true, true)
      | some entries =>
        for entry in entries do
          if entry.fileName == "." || entry.fileName == ".." then continue
          let child := path / entry.fileName
          if opts.maxDepth ≥ 0 && depth >= opts.maxDepth then
            match ← try some <$> lstatAll child.toString catch _ => pure none with
            | some arr2 =>
              if arr2.size ≥ 6 then total := total + arrGet arr2 5
            | none => pure ()
          else
            let (childBlocks, _, _) ← duPath progName child (depth + 1) opts
            total := total + childBlocks
        return (total, true, true)

/--
Run the `du` utility.

Parses arguments, then computes disk usage for each file operand.
Defaults to current directory when no operand is given.
-/
def run (progName : String) (args : List String) : IO UInt32 := do
  let opts := parseArgs args
  let paths := if opts.files.isEmpty then ["."] else opts.files
  let mut failed := false
  for p in paths do
    let path := System.FilePath.mk p
    let (totalBlocks, isDir, ok) ← duPath progName path 0 opts
    if !ok then
      failed := true
    else if opts.summarize || !isDir then
      IO.print (formatLine totalBlocks opts p)
    else if opts.all then
      match ← try some <$> path.readDir catch _ => pure none with
      | some entries =>
        for entry in entries do
          if entry.fileName == "." || entry.fileName == ".." then continue
          let child := path / entry.fileName
          let (childBlocks, _, ok2) ← duPath progName child 0 opts
          if ok2 then
            IO.print (formatLine childBlocks opts (path / entry.fileName |>.toString))
      | none => pure ()
    if isDir && !opts.summarize then
      IO.print (formatLine totalBlocks opts p)
  if failed then
    return 1
  else
    return 0

end Lentils.Du
