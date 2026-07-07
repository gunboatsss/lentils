/-
Wc — IO wrapper for the `wc` utility.
0BSD
-/

import Lentils.Common.Errors
import Lentils.Common.IO.Fd
import Lentils.Wc.Logic

namespace Lentils.Wc

open Lentils.Common.Errors
open Lentils.Common.IO.Fd
open Logic
open ByteArray

partial def readAll (fd : UInt32) (bufSize : USize := 65536) : IO ByteArray := do
  let chunk ← readBytes fd bufSize
  if chunk.isEmpty then return ByteArray.empty
  else return chunk ++ (← readAll fd bufSize)

def run (args : List String) : IO UInt32 := do
  ignoreSigpipe
  let (flags, filenames) := parseArgs args
  match filenames with
  | [] => do
    let content ← readAll 0
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
        let content ← readAll 0
        let l := countLines content
        let w := countWords content
        let c := countBytes content
        totalLines := totalLines + l
        totalWords := totalWords + w
        totalBytes := totalBytes + c
        IO.print (formatCounts l w c "" flags)
        pure ()
      else
        let fdResult ← try
          let fd ← openFile file 0 0
          pure (some fd)
        catch _ =>
          let _ ← exitError "wc" (some file) "No such file or directory"
          exitCode := 1
          pure none
        match fdResult with
        | none => pure ()
        | some fd => do
          let content ← readAll fd
          let l := countLines content
          let w := countWords content
          let c := countBytes content
          totalLines := totalLines + l
          totalWords := totalWords + w
          totalBytes := totalBytes + c
          IO.print (formatCounts l w c file flags)
          closeFd fd
    if files.length > 1 then
      IO.print (formatCounts totalLines totalWords totalBytes "total" flags)
    return exitCode

end Lentils.Wc
