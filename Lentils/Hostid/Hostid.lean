/-
Hostid — IO wrapper for the `hostid` utility. 0BSD -/
import Lentils.Hostid.Logic

namespace Lentils.Hostid

open Logic

/-- FFI: call gethostid() and return as 8-char hex string. -/
@[extern "lean_coreutils_gethostid"]
opaque gethostid : IO String

def run (_args : List String) : IO UInt32 := do
  let id ← gethostid
  IO.println id
  return 0

end Lentils.Hostid
