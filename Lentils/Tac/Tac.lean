/-
Tac — IO wrapper for the `tac` utility.
0BSD

Concatenates files (or stdin) and writes them in reverse line order.
When multiple files are given, each file is reversed individually and
output in file order (matching GNU tac behaviour).

Uses `Tac.Logic` for the pure line-reversal, and `Lentils.Common.IO.Native`
for reading/writing.

Provenance: POSIX.1-2017, Section "tac — concatenate and write files in reverse".
No GPL source was consulted.
-/

import Lentils.Common.Errors
import Lentils.Common.IO.Native
import Lentils.Tac.Logic

namespace Lentils.Tac

open Lentils.Common.Errors
open Lentils.Common.IO.Native
open Logic

/--
Read all bytes from a file. Returns the bytes, or `none` on error (and prints
an error message).
-/
def readFileBytes (prog : String) (path : String) : IO (Option ByteArray) := do
  try
    let f ← openFileRead path
    let content ← readAll f
    return some content
  catch e =>
    IO.eprintln s!"{prog}: {path}: {e.toString}"
    return none

/--
Run the `tac` utility.

With no arguments, reads stdin and reverses its lines.
With file arguments, each file is read, reversed line-by-line, and written
to stdout in file order (matching GNU tac behaviour).
-/
def run (args : List String) : IO UInt32 := do
  ignoreSigpipe
  let prog := "tac"
  let (_opts, files) := parseArgs args

  if files.isEmpty then
    -- Read from stdin, reverse, output
    let input ← readStdin
    try
      writeStdout (reverseBytes input)
      return 0
    catch _ =>
      return 1
  else
    let mut exitCode : UInt32 := 0
    for f in files do
      let bytes ←
        if f = "-" then
          readStdin
        else
          match ← readFileBytes prog f with
          | some b => pure b
          | none =>
            exitCode := 1
            pure ByteArray.empty
      let reversed := reverseBytes bytes
      try
        writeStdout reversed
      catch _ =>
        exitCode := 1
    return exitCode

end Lentils.Tac
