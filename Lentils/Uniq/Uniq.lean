/-
Uniq — IO wrapper for the `uniq` utility.
0BSD
-/

import Lentils.Common.Errors
import Lentils.Common.IO.Fd
import Lentils.Uniq.Logic

namespace Lentils.Uniq

open Lentils.Common.Errors
open Lentils.Common.IO.Fd
open Logic

partial def readAll (fd : UInt32) (bufSize : USize := 65536) : IO ByteArray := do
  let chunk ← readBytes fd bufSize
  if chunk.isEmpty then return ByteArray.empty
  else return chunk ++ (← readAll fd bufSize)

def tryWrite (buf : ByteArray) : IO Bool :=
  try let _ ← writeBytes 1 buf; return true catch _ => return false

def run (args : List String) : IO UInt32 := do
  ignoreSigpipe
  let (mode, filenames) := parseArgs args
  let input ←
    match filenames with
    | [] => readAll 0
    | file :: _ =>
      if file = "-" then readAll 0
      else
        let fdResult ← try
          let fd ← openFile file 0 0
          let content ← readAll fd
          closeFd fd
          pure content
        catch _ =>
          pure ByteArray.empty
        pure fdResult
  let result := processLines input mode
  let ok ← tryWrite result
  return if ok then 0 else 1

end Lentils.Uniq
