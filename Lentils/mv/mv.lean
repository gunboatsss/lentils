/-
Mv — IO wrapper for the `mv` utility.
0BSD

Moves or renames files using `IO.FS.rename`. Supports multiple sources when
the destination is a directory (each source is moved into it, preserving its
base name). Flag handling (`-f`/`-i`/`-n`/`-v`) is parsed but only `-v` has a
visible effect here; `IO.FS.rename` already overwrites the destination.
-/

import Lentils.mv.Logic
import Lentils.Common.Errors

namespace Lentils.mv

open Logic
open Lentils.Common.Errors

/--
Run the `mv` utility.

Parses arguments, then renames each source to its target. When more than one
source is supplied the destination must be a directory; each source is moved
into it under its own base name. Returns exit code 0 on success, or a
non-zero code if any rename fails.
-/
def run (args : List String) : IO UInt32 := do
  let (opts, operands) := parseArgs args
  let (sources, dest?) := splitSourcesDest operands
  match dest? with
  | none =>
    return ← exitUsage "mv" "[-finv] SOURCE... DIRECTORY"
  | some dest =>
    if sources.isEmpty then
      return ← exitUsage "mv" "[-finv] SOURCE... DIRECTORY"
    -- When moving multiple sources the destination must be a directory.
    let destIsDir : Bool ←
      try (System.FilePath.mk dest).isDir catch _ => pure false
    if sources.length > 1 && !destIsDir then
      IO.eprintln s!"mv: target '{dest}' is not a directory"
      return 1
    let mut failed := false
    for src in sources do
      let target := targetPath dest src destIsDir
      try
        IO.FS.rename (System.FilePath.mk src) (System.FilePath.mk target)
        if opts.verbose then
          IO.println s!"renamed '{src}' -> '{target}'"
      catch e =>
        IO.eprintln s!"mv: cannot move '{src}' to '{target}': {e.toString}"
        failed := true
    if failed then
      return 1
    else
      return 0

end Lentils.mv
