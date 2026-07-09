/-
Grep — IO wrapper for the `grep` utility. 0BSD
-/

import Lentils.Common.Errors
import Lentils.Common.IO.Fd
import Lentils.Grep.Logic

namespace Lentils.Grep

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
  let (flags, pattern, filenames) := parseArgs args
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
  let (result, hasMatch) := processInput input pattern flags
  let ok ← tryWrite result
  -- POSIX: exit 0 if match found, 1 if no match, 2 if error
  if not ok then return 2
  else if not hasMatch then return 1
  else return 0

end Lentils.Grep
