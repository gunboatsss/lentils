/-
Cp — IO wrapper for the `cp` utility.
0BSD

Copies files using `IO.FS.readBinFile` / `IO.FS.writeBinFile`. With `-r`/`-R`
directories are copied recursively (using `System.FilePath.readDir` and
`System.FilePath.metadata`). Multiple sources require the destination to be a
directory; each source is copied into it under its own base name. Flag
handling (`-f`/`-r`/`-v`) is parsed but `-f` is honoured implicitly because
`writeBinFile` overwrites the destination.
-/

import Lentils.Cp.Logic
import Lentils.Common.Errors

namespace Lentils.Cp

open Logic
open Lentils.Common.Errors

/--
Copy a single regular file from `src` to `dst`.
Returns `true` on success, `false` on failure.
-/
def copyFile (src dst : System.FilePath) : IO Bool := do
  try
    let content ← IO.FS.readBinFile src
    IO.FS.writeBinFile dst content
    return true
  catch e =>
    IO.eprintln s!"cp: cannot copy '{src.toString}' to '{dst.toString}': {e.toString}"
    return false

/--
Recursively copy `src` to `dst`.
Directories are recreated and their entries copied entry-by-entry; regular
files are copied byte-for-byte. Returns `true` on success, `false` on failure.
-/
partial def copyRecursive (src dst : System.FilePath) : IO Bool := do
  match ← try some <$> src.metadata catch _ => pure none with
  | none =>
      IO.eprintln s!"cp: cannot stat '{src.toString}'"
      return false
  | some m =>
    if m.type == .dir then
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
      return ← exitUsage "cp" "[-frRv] SOURCE... DIRECTORY"
  | some dest =>
    if sources.isEmpty then
      return ← exitUsage "cp" "[-frRv] SOURCE... DIRECTORY"
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
