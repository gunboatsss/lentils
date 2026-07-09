/-
Tr — IO wrapper for the `tr` utility. 0BSD
-/

import Lentils.Common.Errors
import Lentils.Common.IO.Fd
import Lentils.Tr.Logic

namespace Lentils.Tr

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
  let (mode, set1, set2) := parseArgs args
  let input ← readAll 0
  let result := processInput input mode set1 set2
  let ok ← tryWrite result
  return if ok then 0 else 1

end Lentils.Tr
