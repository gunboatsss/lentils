/-
Ln — IO wrapper for the `ln` utility.
0BSD

Creates links between files. With `-s`/`--symbolic` a symbolic link is made
via the C FFI call `symlink(2)`; otherwise a hard link is made via `link(2)`.
With `-f`/`--force` an existing destination is removed first via `unlink(2)`.
-/

import Lentils.Ln.Logic
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Ln

open Logic
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/--
Create a single link (symbolic or hard) from `target` to `link`.

With `-f`/`--force` the destination is unlinked first. The link kind is chosen
by `opts.symbolic`. Returns `true` on success, `false` on failure.
-/
def makeLink (opts : Options) (target linkName : String) : IO Bool := do
  if opts.force then
    try unlink linkName catch _ => pure ()
  let ok : Bool ←
    if opts.symbolic then
      try
        symlink target linkName
        pure true
      catch e =>
        IO.eprintln s!"ln: failed to create symbolic link '{linkName}' -> '{target}': {e.toString}"
        pure false
    else
      try
        link target linkName
        pure true
      catch e =>
        IO.eprintln s!"ln: failed to create hard link '{linkName}' -> '{target}': {e.toString}"
        pure false
  if ok && opts.verbose then
    IO.println s!"'{linkName}' -> '{target}'"
  return ok

/--
Compute the link name for a source when linking into a directory.
-/
def linkNameInDir (dirName source : String) : String :=
  (System.FilePath.mk dirName / (System.FilePath.mk source).fileName.getD source).toString

/--
Run the `ln` utility.

Parses arguments (handling `-s`/`--symbolic`, `-f`/`--force`, `-v`/`--verbose`).
The last operand is the link name (or target directory when more than one
source is given); with a single operand the link is created in the current
directory under the source's base name. Returns exit code 0 on success, or a
non-zero code if any link cannot be created.
-/
def run (args : List String) : IO UInt32 := do
  let (opts, operands) := parseArgs args
  if operands.isEmpty then
    return ← exitUsage "ln" "[-sfv] TARGET [LINK]"
  let (sources, linkName?) := splitSourcesLink operands
  match linkName? with
  | none =>
      -- single operand: link in cwd under base name
      let target := sources.head!
      let link := (System.FilePath.mk target).fileName.getD target
      if !(← makeLink opts target link) then
        return 1
      else
        return 0
  | some linkName =>
      if sources.length > 1 then
        let isDir : Bool ← try (System.FilePath.mk linkName).isDir catch _ => pure false
        if !isDir then
          IO.eprintln s!"ln: target '{linkName}' is not a directory"
          return 1
        let mut failed := false
        for s in sources do
          if !(← makeLink opts s (linkNameInDir linkName s)) then
            failed := true
        return if failed then 1 else 0
      else
        let target := sources.head!
        if !(← makeLink opts target linkName) then
          return 1
        else
          return 0

end Lentils.Ln
