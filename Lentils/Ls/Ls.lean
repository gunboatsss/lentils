/-
Lentils.Ls — IO wrapper for the `ls` utility.
0BSD

Enumerates directory contents with `System.FilePath.readDir` and reads
file metadata (backed by `stat`) with `System.FilePath.metadata`.
All argument parsing and output formatting is delegated to the pure
`Lentils.Ls.Logic` layer.
-/

import Lentils.Ls.Logic
import Lentils.Common.Errors

namespace Lentils.Ls

open Logic
open Lentils.Common.Errors
open IO.FS

/--
Map a `FileType` to the single-character type indicator used by `ls -l`.
-/
def typeChar (t : FileType) : String :=
  match t with
  | .dir     => "d"
  | .symlink => "l"
  | .other   => "?"
  | .file    => "-"

/--
Read a directory's entries, returning `none` on failure
(not a directory, permission denied, …).
-/
def readEntries (path : System.FilePath) : IO (Option (Array DirEntry)) :=
  try some <$> path.readDir catch _ => pure none

/--
Read a single file's metadata, returning `none` on failure.
-/
def getMeta (path : System.FilePath) : IO (Option Metadata) :=
  try some <$> path.metadata catch _ => pure none

/--
List the contents of one directory operand according to `opts`.
-/
def listDir (opts : Options) (dir : String) : IO UInt32 := do
  let path := System.FilePath.mk dir
  match ← readEntries path with
  | none =>
      let _ ← exitError "ls" (some dir) "cannot read directory"
      return 1
  | some entries =>
      let mut names : List String := []
      for d in entries do
        if showName opts d.fileName then
          names := names ++ [d.fileName]
      let sorted := sortNames names
      if opts.long then
        for nm in sorted do
          match ← getMeta (path / nm) with
          | some m =>
              IO.print (formatLongLine
                { typeChar := typeChar m.type
                  links := m.numLinks
                  size := m.byteSize
                  modifiedSec := m.modified.sec
                  name := nm })
          | none => IO.print (formatName nm)
      else
        for nm in sorted do
          IO.print (formatName nm)
      return 0

/--
List a single non-directory operand (a plain file or symlink).
-/
def listFile (opts : Options) (target : String) : IO UInt32 := do
  let path := System.FilePath.mk target
  match ← getMeta path with
  | none =>
      let _ ← exitError "ls" (some target) "No such file or directory"
      return 1
  | some m =>
      if opts.long then
        IO.print (formatLongLine
          { typeChar := typeChar m.type
            links := m.numLinks
            size := m.byteSize
            modifiedSec := m.modified.sec
            name := target })
      else
        IO.print (formatName target)
      return 0

/--
Run the `ls` utility.
Lists each operand: directories are enumerated, plain files are listed
by name. Returns exit code 0 on success, 1 if any operand could not be
listed.
-/
def run (args : List String) : IO UInt32 := do
  let opts := parseArgs args
  let mut exitCode : UInt32 := 0
  for target in opts.dirs do
    let path := System.FilePath.mk target
    let isd ← try path.isDir catch _ => pure false
    if isd then
      let code ← listDir opts target
      if code ≠ 0 then exitCode := code
    else
      let code ← listFile opts target
      if code ≠ 0 then exitCode := code
  return exitCode

end Lentils.Ls
