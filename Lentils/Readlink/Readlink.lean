/-
Readlink — IO wrapper for the `readlink` utility. 0BSD -/
import Lentils.Readlink.Logic

namespace Lentils.Readlink

open Logic

/-- FFI: readlink(2) — read symlink target. -/
@[extern "lean_coreutils_readlink"]
opaque readlink (path : String) : IO String

def run (args : List String) : IO UInt32 := do
  match getPath args with
  | none =>
    IO.eprintln "Usage: lentils readlink path"
    return 1
  | some path =>
    try
      let target ← readlink path
      IO.println target
      return 0
    catch _ =>
      IO.eprintln s!"readlink: {path}: No such file or directory"
      return 1

end Lentils.Readlink
