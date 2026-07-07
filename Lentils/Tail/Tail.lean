/-
Tail — IO wrapper for the `tail` utility.
0BSD
-/

import Lentils.Common.Errors
import Lentils.Common.IO.Fd
import Lentils.Tail.Logic

namespace Lentils.Tail

open Lentils.Common.Errors
open Lentils.Common.IO.Fd
open Logic

partial def readAll (fd : UInt32) (bufSize : USize := 65536) : IO ByteArray := do
  let chunk ← readBytes fd bufSize
  if chunk.isEmpty then return ByteArray.empty
  else return chunk ++ (← readAll fd bufSize)

def tryWrite (buf : ByteArray) : IO Bool :=
  try let _ ← writeBytes 1 buf; return true catch _ => return false

def filterFilenames (args : List String) : List String :=
  let rec go (args : List String) : List String :=
    match args with
    | [] => []
    | "-n" :: _ :: rest => go rest
    | arg :: rest =>
      if arg.startsWith "-n" then go rest
      else if arg.startsWith "-" && arg.length > 1 &&
        (arg.drop 1).toString.all (·.isDigit) then go rest
      else arg :: go rest
  go args

def run (args : List String) : IO UInt32 := do
  ignoreSigpipe
  let count ← match parseCount args with
    | some n => pure n
    | none => return 1
  let filenames := filterFilenames args
  match filenames with
  | [] => do
    let content ← readAll 0
    let result := takeLastLines content count
    let ok ← tryWrite result
    return if ok then 0 else 1
  | files =>
    let mut exitCode : UInt32 := 0
    for file in files do
      if file = "-" then
        let content ← readAll 0
        let result := takeLastLines content count
        let ok ← tryWrite result
        if not ok then exitCode := 1
        pure ()
      else
        let fdResult ← try
          let fd ← openFile file 0 0
          pure (some fd)
        catch _ =>
          let _ ← exitError "tail" (some file) "No such file or directory"
          exitCode := 1
          pure none
        match fdResult with
        | none => pure ()
        | some fd => do
          let content ← readAll fd
          let result := takeLastLines content count
          let ok ← tryWrite result
          if not ok then exitCode := 1
          closeFd fd
    return exitCode

end Lentils.Tail
