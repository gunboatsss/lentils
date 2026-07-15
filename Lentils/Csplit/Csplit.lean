/-
Csplit — IO wrapper for the `csplit` utility.
0BSD

Splits a file into pieces at specified patterns.
Writes pieces to files (xx00, xx01, ...) and prints byte counts.

Provenance: POSIX.1-2017, Section "csplit — split files based on context".
No GPL source was consulted.
-/

import Lentils.Csplit.Logic
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Csplit

open Logic
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/--
Format an output filename.
-/
def formatFileName (opts : Options) (index : Nat) : String :=
  let padded :=
    let s := toString index
    if s.length >= opts.digits then s
    else String.ofList (List.replicate (opts.digits - s.length) '0') ++ s
  opts.filePrefix ++ padded

/--
Read lines from a file, stripping trailing empty line from splitOn.
-/
def readLines (path : String) : IO (List String) := do
  let content ← IO.FS.readFile path
  let allLines := content.splitOn "\n"
  if allLines.length > 0 && allLines[allLines.length - 1]! = "" then
    pure (allLines.take (allLines.length - 1))
  else
    pure allLines

/--
Run the `csplit` utility.

Parses arguments, reads the input file, splits at patterns,
writes pieces to numbered output files, and prints byte counts.
-/
def run (args : List String) : IO UInt32 := do
  let (opts, file, patterns) := parseArgs args
  if file.isEmpty then
    return ← exitUsage "csplit" "missing file operand"
  if patterns.isEmpty then
    return ← exitUsage "csplit" "missing pattern"

  -- Read input lines
  let mut failed := false
  let lines : List String ←
    if file = "-" then
      readStdinLines
    else
      try readLines file
      catch e =>
        IO.eprintln s!"csplit: {file}: {e.toString}"
        failed := true
        pure []

  -- Compute splits and pieces
  let splits := computeSplits lines patterns
  let pieces := splitLines lines splits

  -- Write pieces
  let mut index := 0
  for piece in pieces do
    if piece.isEmpty && opts.elideEmpty then
      continue
    let filename := formatFileName opts index
    let content := String.intercalate "\n" piece
    let content' := if content.isEmpty then content else content ++ "\n"
    try
      IO.FS.writeFile filename content'
      if !opts.quiet then
        IO.println (toString content'.length)
    catch e =>
      IO.eprintln s!"csplit: {filename}: {e.toString}"
      failed := true
      if !opts.keepFiles then
        break
    index := index + 1

  return if failed then 1 else 0

end Lentils.Csplit