/-
Install — IO wrapper for the `install` utility.
0BSD

Copies files and sets permissions, optionally creating directories.
Combines cp + stat + chmod + mkdir functionality.

Supports:
  -d  create directories
  -m  set permission mode
  -v  verbose
  -C  compare (skip copy if source and dest are identical)
  -b  backup existing files before overwriting
  -s  strip symbols (not implemented — passes through)
  -o  set owner (via chown)
  -g  set group (via chgrp)
-/

import Lentils.Install.Logic
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Install

open Logic
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/--
Copy a single regular file from `src` to `dst`.
Returns `true` on success.
-/
def copyFile (src dst : System.FilePath) : IO Bool := do
  try
    let content ← IO.FS.readBinFile src
    IO.FS.writeBinFile dst content
    return true
  catch e =>
    IO.eprintln s!"install: cannot copy '{src.toString}' to '{dst.toString}': {e.toString}"
    return false

/--
Compare two files by content. Returns `true` if they are identical.
-/
def compareFiles (src dst : System.FilePath) : IO Bool := do
  try
    let srcContent ← IO.FS.readBinFile src
    let dstContent ← IO.FS.readBinFile dst
    return srcContent == dstContent
  catch _ =>
    return false

/--
Backup an existing file by renaming it with a `~` suffix.
-/
def backupFile (path : System.FilePath) : IO Bool := do
  try
    let backupPath := path.toString ++ "~"
    IO.FS.rename path (System.FilePath.mk backupPath)
    return true
  catch e =>
    IO.eprintln s!"install: cannot backup '{path.toString}': {e.toString}"
    return false

/--
Apply the specified mode to a file.
-/
def applyMode (path : String) (mode : UInt32) : IO Bool := do
  try
    chmod path mode
    return true
  catch e =>
    IO.eprintln s!"install: cannot set permissions on '{path}': {e.toString}"
    return false

/--
Set owner (chown-like) using POSIX chown via FFI.
-/
@[extern "lean_coreutils_chown"]
opaque chown (path : String) (owner : String) (group : String) : IO Unit

/--
Set owner if -o was given.
-/
def applyOwner (path : String) (owner : Option String) : IO Bool := do
  match owner with
  | none => return true
  | some o =>
    try
      chown path o ""
      return true
    catch e =>
      IO.eprintln s!"install: cannot set owner on '{path}': {e.toString}"
      return false

/--
Set group if -g was given.
-/
def applyGroup (path : String) (group : Option String) : IO Bool := do
  match group with
  | none => return true
  | some g =>
    try
      chown path "" g
      return true
    catch e =>
      IO.eprintln s!"install: cannot set group on '{path}': {e.toString}"
      return false

/--
Create a directory (including parents) with the specified mode.
-/
def installDir (path : String) (mode : UInt32) : IO Bool := do
  try
    IO.FS.createDirAll (System.FilePath.mk path)
    chmod path mode
    return true
  catch e =>
    IO.eprintln s!"install: cannot create directory '{path}': {e.toString}"
    return false

/--
Install a single source file to a destination.
Handles -C (compare), -b (backup), -s (strip), -o, -g.
-/
def installFile (progName : String) (src dst : System.FilePath) (opts : Options) : IO Bool := do
  -- Create parent directories if needed
  try
    let parent := dst.parent
    match parent with
    | some p => IO.FS.createDirAll p
    | none => pure ()
  catch _ => pure ()

  -- Check if destination exists for -C (compare) and -b (backup)
  let destExists ← try dst.pathExists catch _ => pure false

  -- For -C: skip if files are identical
  if opts.compare && destExists then
    if (← compareFiles src dst) then
      if opts.verbose then
        IO.println s!"{progName}: '{src.toString}' and '{dst.toString}' are identical"
      return true

  -- For -b: backup existing destination
  if opts.backup && destExists then
    _ ← backupFile dst

  -- Copy the file
  let ok ← copyFile src dst
  if !ok then return false

  -- Apply mode
  if !(← applyMode dst.toString opts.mode) then return false

  -- Apply owner (-o)
  if !(← applyOwner dst.toString opts.owner) then return false

  -- Apply group (-g)
  if !(← applyGroup dst.toString opts.group) then return false

  -- -s (strip) not implemented — warn and continue
  if opts.strip then
    IO.eprintln s!"{progName}: warning: --strip not implemented for '{src.toString}'"

  if opts.verbose then
    IO.println s!"{progName}: '{src.toString}' -> '{dst.toString}'"
  return true

/--
Run the `install` utility.

In copy mode (`-d` not given): copies each source to the destination,
setting permissions as specified by `-m` (default 0755).
In directory mode (`-d`): creates each operand as a directory.
Returns exit code 0 on success, or non-zero if any operation fails.
-/
def run (progName : String) (args : List String) : IO UInt32 := do
  let opts := parseArgs args
  let (sources, dest?) := splitOperands opts
  if opts.directory then
    -- Create directories
    if opts.operands.isEmpty then
      return ← exitUsage progName "missing file operand"
    let mut failed := false
    for d in opts.operands do
      if !(← installDir d opts.mode) then
        failed := true
    if failed then return 1 else return 0
  else
    -- Copy mode
    match dest? with
    | none =>
        return ← exitUsage progName "missing file operand"
    | some dest =>
      if sources.isEmpty then
        return ← exitUsage progName "missing file operand"
      let destPath := System.FilePath.mk dest
      let destIsDir : Bool ←
        try destPath.isDir catch _ => pure false
      let mut failed := false
      for src in sources do
        let srcPath := System.FilePath.mk src
        let target : System.FilePath :=
          if destIsDir then
            destPath / (srcPath.fileName.getD src)
          else
            destPath
        if !(← installFile progName srcPath target opts) then
          failed := true
      if failed then return 1 else return 0

end Lentils.Install
