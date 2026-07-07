/-
Tee — IO wrapper for the `tee` utility.
0BSD
-/

import Lentils.Common.Errors
import Lentils.Common.IO.Fd
import Lentils.Tee.Logic

namespace Lentils.Tee

open Lentils.Common.Errors
open Lentils.Common.IO.Fd
open Logic
open ByteArray

partial def readAll (fd : UInt32) (bufSize : USize := 65536) : IO ByteArray := do
  let chunk ← readBytes fd bufSize
  if chunk.isEmpty then return ByteArray.empty
  else return chunk ++ (← readAll fd bufSize)

def tryWriteFd (fd : UInt32) (buf : ByteArray) : IO Bool :=
  try let _ ← writeBytes fd buf; return true catch _ => return false

def openOutputFile (path : String) (append : Bool) : IO (Option UInt32) :=
  let flags := if append then O_WRONLY ||| O_CREAT ||| O_APPEND
                         else O_WRONLY ||| O_CREAT ||| O_TRUNC
  try let fd ← openFile path flags DEFAULT_MODE; return some fd catch _ => return none

def run (args : List String) : IO UInt32 := do
  ignoreSigpipe
  let append := parseAppend args
  let filenames := parseFilenames args
  let input ← readAll 0
  let mut fds : List UInt32 := []
  let mut exitCode : UInt32 := 0
  for file in filenames do
    let fdResult ← openOutputFile file append
    match fdResult with
    | none =>
      let _ ← exitError "tee" (some file) "Failed to open"
      exitCode := 1
    | some fd => fds := fd :: fds
  let stdoutOk ← tryWriteFd 1 input
  if not stdoutOk then exitCode := 1
  for fd in fds do
    let ok ← tryWriteFd fd input
    if not ok then exitCode := 1
    closeFd fd
  return exitCode

end Lentils.Tee
