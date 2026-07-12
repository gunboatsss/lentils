/-
Cat — IO wrapper for `cat` utility.

Reads from stdin and/or files, concatenates, writes to stdout.
Uses Cat.Logic (pure, verified) for byte processing.
Uses Lean native IO (IO.FS.Handle) instead of C FFI.
-/

import Lentils.Common.Errors
import Lentils.Common.IO.Native
import Lentils.Cat.Logic

namespace Lentils.Cat

open Lentils.Common.Errors
open Lentils.Common.IO.Native

/-- Read from a file, process bytes, and write to stdout.
    Returns true if write succeeded. -/
def processFile (f : File) : IO Bool := do
  let content ← readAll f
  let processed := Logic.processBytes content
  try
    writeStdout processed
    return true
  catch _ =>
    return false

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

  match args with
  | [] => do
    -- Read from stdin
    let content ← readStdin
    let processed := Logic.processBytes content
    try
      writeStdout processed
      return 0
    catch _ =>
      return 1

  | files =>
    let mut exitCode : UInt32 := 0
    for file in files do
      if file = "-" then
        let content ← readStdin
        let processed := Logic.processBytes content
        try
          writeStdout processed
        catch _ =>
          exitCode := 1
        pure ()
      else
        let fileResult ←
          try
            let f ← openFileRead file
            pure (some f)
          catch _ =>
            let _ ← exitError prog (some file) "No such file or directory"
            exitCode := 1
            pure none
        match fileResult with
        | none   => pure ()
        | some f => do
          let ok ← processFile f
          if not ok then exitCode := 1
          -- File handle is GC-managed, no explicit close needed
    return exitCode

end Lentils.Cat
