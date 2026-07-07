/-
Cat — IO wrapper for `cat` utility.

Reads from stdin and/or files, concatenates, writes to stdout.
Uses Cat.Logic (pure, verified) for byte processing.
IO/FFI side effects are confined to this module.
-/

import Lentils.Common.Errors
import Lentils.Common.IO.Fd
import Lentils.Cat.Logic

namespace Lentils.Cat

open Lentils.Common.Errors
open Lentils.Common.IO.Fd

/-- Read the entire contents of an open file descriptor. -/
partial def readAll (fd : UInt32) (bufSize : USize := 65536) : IO ByteArray := do
  let chunk ← readBytes fd bufSize
  if chunk.isEmpty then
    return ByteArray.empty
  else
    let rest ← readAll fd bufSize
    return chunk ++ rest

/-- Write bytes to fd, catching errors to avoid segfault from malformed FFI errors.
    Returns true if write succeeded, false otherwise.
    POSIX cat continues after write errors but sets non-zero exit. -/
def tryWrite (fd : UInt32) (buf : ByteArray) : IO Bool := do
  try
    let _ ← writeBytes fd buf
    return true
  catch _ =>
    return false

/-- Read from an fd, process bytes, and write to stdout fd.
    Returns true if write succeeded. -/
def processFd (fd : UInt32) (stdoutFd : UInt32) : IO Bool := do
  let content ← readAll fd
  let processed := Logic.processBytes content
  tryWrite stdoutFd processed

/-- Run `cat` with the given arguments.
    Returns UInt32 exit code (0 on success, 1 on error).
    Handles:
    - No args: read from stdin
    - "-": read from stdin
    - Files: open each file, read, write to stdout -/
def run (args : List String) : IO UInt32 := do
  -- Ignore SIGPIPE so we don't crash when stdout is a closed pipe.
  ignoreSigpipe

  let prog : String := "cat"
  let stdoutFd : UInt32 := 1  -- STDOUT_FILENO

  match args with
  | [] => do
    -- Read from stdin (fd 0)
    let content ← readAll 0
    let processed := Logic.processBytes content
    let ok ← tryWrite stdoutFd processed
    if ok then return 0 else return 1

  | files =>
    let mut exitCode : UInt32 := 0
    for file in files do
      if file = "-" then
        let content ← readAll 0
        let processed := Logic.processBytes content
        let ok ← tryWrite stdoutFd processed
        if not ok then exitCode := 1
        pure ()
      else
        let fdResult ←
          try
            let fd ← openFile file 0 0  -- O_RDONLY = 0, mode = 0
            pure (some fd)
          catch _ =>
            let _ ← exitError prog (some file) "No such file or directory"
            exitCode := 1
            pure none
        match fdResult with
        | none   => pure ()
        | some fd => do
          let ok ← processFd fd stdoutFd
          if not ok then exitCode := 1
          closeFd fd
    return exitCode

end Lentils.Cat
