/-
Hostid — IO wrapper for the `hostid` utility. 0BSD -/
import Lentils.Hostid.Logic

namespace Lentils.Hostid

open Logic

def run (_args : List String) : IO UInt32 := do
  let raw ←
    try IO.FS.readFile "/proc/sys/kernel/hostid"
    catch _ => pure ""
  let id := formatHostid raw
  IO.println id
  return 0

end Lentils.Hostid
