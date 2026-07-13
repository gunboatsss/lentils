/-
Nl — IO wrapper for the `nl` utility. 0BSD -/
import Lentils.Nl.Logic

namespace Lentils.Nl

open Logic

def run (_args : List String) : IO UInt32 := do
  let input ←
    try IO.FS.readFile "/dev/stdin"
    catch _ => pure ""

  let result := numberLines input
  IO.print result
  return 0

end Lentils.Nl
