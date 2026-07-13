/-
Realpath — IO wrapper for the `realpath` utility. 0BSD -/
import Lentils.Realpath.Logic

namespace Lentils.Realpath

open Logic

/-- FFI: realpath(3) — resolve path to canonical absolute path. -/
@[extern "lean_coreutils_realpath"]
opaque realpath (path : String) : IO String

def run (args : List String) : IO UInt32 := do
  match getPath args with
  | none =>
    IO.eprintln "Usage: lentils realpath path"
    return 1
  | some path =>
    try
      let resolved ← realpath path
      IO.println resolved
      return 0
    catch _ =>
      IO.eprintln s!"realpath: {path}: No such file or directory"
      return 1

end Lentils.Realpath
