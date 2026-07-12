/-
Wc — IO wrapper for the `wc` utility.
0BSD
-/

import Lentils.Common.Errors
import Lentils.Common.IO.Native
import Lentils.Wc.Logic

namespace Lentils.Wc

open Lentils.Common.Errors
open Lentils.Common.IO.Native
open Logic
open ByteArray

def run (args : List String) : IO UInt32 := do
  ignoreSigpipe
  let (flags, filenames) := parseArgs args
  match filenames with
  | [] => do
    let content ← readStdin
    let l := countLines content
    let w := countWords content
    let c := countBytes content
    IO.print (formatCounts l w c "" flags)
    return 0
  | files => do
    let mut exitCode : UInt32 := 0
    let mut totalLines : Nat := 0
    let mut totalWords : Nat := 0
    let mut totalBytes : Nat := 0
    for file in files do
      if file = "-" then
        let content ← readStdin
        let l := countLines content
        let w := countWords content
        let c := countBytes content
        totalLines := totalLines + l
        totalWords := totalWords + w
        totalBytes := totalBytes + c
        IO.print (formatCounts l w c "" flags)
        pure ()
      else
        let fileResult ← try
          let f ← openFileRead file
          pure (some f)
        catch _ =>
          let _ ← exitError "wc" (some file) "No such file or directory"
          exitCode := 1
          pure none
        match fileResult with
        | none => pure ()
        | some f => do
          let content ← readAll f
          let l := countLines content
          let w := countWords content
          let c := countBytes content
          totalLines := totalLines + l
          totalWords := totalWords + w
          totalBytes := totalBytes + c
          IO.print (formatCounts l w c file flags)
    if files.length > 1 then
      IO.print (formatCounts totalLines totalWords totalBytes "total" flags)
    return exitCode

end Lentils.Wc
