/-
Cp — IO wrapper for the `cp` utility.
0BSD

Copies files using `IO.FS.readBinFile` / `IO.FS.writeBinFile`. With `-r`/`-R`
directories are copied recursively (using `System.FilePath.readDir` and
`lstatAll`). Multiple sources require the destination to be a directory; each
source is copied into it under its own base name. Flag handling
(`-f`/`-r`/`-v`) is parsed but `-f` is honoured implicitly because
`writeBinFile` overwrites the destination.
-/

import Lentils.Cp.Logic
import Lentils.Common.Array
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Cp

open Logic
open Lentils.Common.Array
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/-- Determine file type nibble from lstat mode bits. -/
def fileType (mode : UInt64) : UInt64 :=
  (mode >>> 12) &&& 0xF

/--
Check if two paths refer to the same file by comparing device and inode.
Returns `true` if they are the same (i.e., copying would be a self-to-self).
-/
def sameFile (path1 path2 : String) : IO Bool := do
  try
    let a1 ← lstatAll path1
    let a2 ← lstatAll path2
    let dev1 := a1[7]!
    let ino1 := a1[8]!
    let dev2 := a2[7]!
    let ino2 := a2[8]!
    return (dev1 == dev2 && ino1 == ino2)
  catch _ =>
    return false

/--
Copy a single regular file from `src` to `dst`.
Preserves source permission bits via stat/chmod.
Returns `true` on success, `false` on failure.
-/
def copyFile (src dst : System.FilePath) : IO Bool := do
  -- Check source existence first via lstat (gives proper errno for error message)
  match ← try some <$> lstatAll src.toString catch _ => pure none with
  | none =>
    IO.eprintln s!"cp: cannot stat '{src.toString}': No such file or directory"
    return false
  | some _ =>
    try
      let content ← IO.FS.readBinFile src
      IO.FS.writeBinFile dst content
      let srcMode ← try statMode src.toString catch _ => pure 0
      if srcMode != 0 then
        try chmod dst.toString srcMode catch _ => pure ()
      return true
    catch e =>
      IO.eprintln s!"cp: cannot stat '{src.toString}': {formatIoError e.toString}"
      return false

/--
Recursively copy `src` to `dst` using lstat semantics.
Symlinks are replicated (readlink + symlink), never followed.
Directories are recreated and their entries copied entry-by-entry; regular
files are copied byte-for-byte with permissions preserved.
Returns `true` on success, `false` on failure.
-/
partial def copyRecursive (src dst : System.FilePath) : IO Bool := do
  match ← try some <$> lstatAll src.toString catch _ => pure none with
  | none =>
      IO.eprintln s!"cp: cannot stat '{src.toString}': No such file or directory"
      return false
  | some arr =>
    let typ := fileType (arrGet arr 0)
    if typ == 0xA then
      -- Symlink: replicate the link itself
      try
        let target ← readlink src.toString
        symlink target dst.toString
        return true
      catch e =>
        IO.eprintln s!"cp: cannot copy '{src.toString}' to '{dst.toString}': {formatIoError e.toString}"
        return false
    else if typ == 0x4 then
      -- Directory (not a symlink)
      try IO.FS.createDirAll dst catch _ => pure ()
      match ← try some <$> src.readDir catch _ => pure none with
      | none =>
          IO.eprintln s!"cp: cannot read directory '{src.toString}'"
          return false
      | some entries =>
          let mut ok := true
          for e in entries do
            if e.fileName == "." || e.fileName == ".." then
              continue
            let s := src / e.fileName
            let d := dst / e.fileName
            if !(← copyRecursive s d) then ok := false
          return ok
    else
      -- Regular file (or other non-symlink, non-directory type)
      return ← copyFile src dst

/--
Run the `cp` utility.

Parses arguments, then copies each source to its target. When more than one
source is supplied the destination must be a directory; each source is copied
into it under its own base name. Returns exit code 0 on success, or a
non-zero code if any copy fails.
-/
def run (args : List String) : IO UInt32 := do
  let (opts, operands) := parseArgs args
  let (sources, dest?) := splitSourcesDest operands
  match dest? with
  | none =>
      IO.eprintln "cp: missing file operand"
      IO.eprintln "Try 'cp --help' for more information."
      return 1
  | some dest =>
    if sources.isEmpty then
      IO.eprintln "cp: missing file operand"
      IO.eprintln "Try 'cp --help' for more information."
      return 1
    let destPath := System.FilePath.mk dest
    let destIsDir : Bool ←
      try destPath.isDir catch _ => pure false
    if sources.length > 1 && !destIsDir then
      IO.eprintln s!"cp: target '{dest}' is not a directory"
      return 1
    let mut failed := false
    for src in sources do
      let srcPath := System.FilePath.mk src
      let target : System.FilePath :=
        if destIsDir then
          destPath / (srcPath.fileName.getD src)
        else
          destPath
      -- Check for self-to-self copy
      if !destIsDir then
        if ← sameFile srcPath.toString target.toString then
          IO.eprintln s!"cp: '{src}' and '{dest}' are the same file"
          failed := true
          continue
      let ok ←
        if opts.recursive then
          copyRecursive srcPath target
        else
          copyFile srcPath target
      if opts.verbose then
        IO.println s!"'{src}' -> '{target.toString}'"
      if !ok then failed := true
    if failed then
      return 1
    else
      return 0

end Lentils.Cp
