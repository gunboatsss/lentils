/-
Tee — IO wrapper for the `tee` utility.
0BSD
-/

import Lentils.Common.Errors
import Lentils.Common.IO.Native
import Lentils.Tee.Logic

namespace Lentils.Tee

open Lentils.Common.Errors
open Lentils.Common.IO.Native
open Logic
open ByteArray

def openOutputFile (path : String) (append : Bool) : IO (Option File) :=
  try
    let f := if append then openFileAppend path else openFileWrite path
    pure (some (← f))
  catch _ =>
    pure none

def run (args : List String) : IO UInt32 := do
  ignoreSigpipe
  let append := parseAppend args
  let filenames := parseFilenames args
  let input ← readStdin
  let mut files : List File := []
  let mut exitCode : UInt32 := 0
  for file in filenames do
    let fResult ← openOutputFile file append
    match fResult with
    | none =>
      let _ ← exitError "tee" (some file) "Failed to open"
      exitCode := 1
    | some f => files := f :: files
  -- Write to stdout
  try
    writeStdout input
  catch _ =>
    exitCode := 1
  -- Write to each output file
  for f in files do
    try
      writeBytes f input
    catch _ =>
      exitCode := 1
  return exitCode

end Lentils.Tee
