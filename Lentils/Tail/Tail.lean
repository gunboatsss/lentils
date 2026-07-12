/-
Tail — IO wrapper for the `tail` utility.
0BSD
-/

import Lentils.Common.Errors
import Lentils.Common.IO.Native
import Lentils.Tail.Logic

namespace Lentils.Tail

open Lentils.Common.Errors
open Lentils.Common.IO.Native
open Logic

def tryWriteStdout (buf : ByteArray) : IO Bool :=
  try writeStdout buf; return true catch _ => return false

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
    let content ← readStdin
    let result := takeLastLines content count
    let ok ← tryWriteStdout result
    return if ok then 0 else 1
  | files =>
    let mut exitCode : UInt32 := 0
    for file in files do
      if file = "-" then
        let content ← readStdin
        let result := takeLastLines content count
        let ok ← tryWriteStdout result
        if not ok then exitCode := 1
        pure ()
      else
        let fileResult ← try
          let f ← openFileRead file
          pure (some f)
        catch _ =>
          let _ ← exitError "tail" (some file) "No such file or directory"
          exitCode := 1
          pure none
        match fileResult with
        | none => pure ()
        | some f => do
          let content ← readAll f
          let result := takeLastLines content count
          let ok ← tryWriteStdout result
          if not ok then exitCode := 1
    return exitCode

end Lentils.Tail
