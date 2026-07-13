/-
Unexpand — IO wrapper for the `unexpand` utility. 0BSD -/
import Lentils.Unexpand.Logic

namespace Lentils.Unexpand

open Logic

def run (_args : List String) : IO UInt32 := do
  let input ←
    try IO.FS.readFile "/dev/stdin"
    catch _ => pure ""
  let result := unexpand input
  IO.print result
  return 0

end Lentils.Unexpand
