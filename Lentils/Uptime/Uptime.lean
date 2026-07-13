/-
Uptime — IO wrapper for the `uptime` utility. 0BSD -/
import Lentils.Uptime.Logic

namespace Lentils.Uptime

open Logic

def run (_args : List String) : IO UInt32 := do
  let content ←
    try IO.FS.readFile "/proc/uptime"
    catch _ => pure ""
  if content.isEmpty then
    IO.eprintln "uptime: cannot read /proc/uptime"
    return 1
  let seconds := parseUptime content
  let formatted := formatUptime seconds
  IO.println formatted
  return 0

end Lentils.Uptime
