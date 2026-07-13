/-
Unexpand — IO wrapper for the `unexpand` utility. 0BSD -/
import Lentils.Common.IO.Native
import Lentils.Unexpand.Logic

namespace Lentils.Unexpand

open Logic
open Lentils.Common.IO.Native

def run (_args : List String) : IO UInt32 := do
  let input ← readStdinText
  let result := unexpand input
  IO.print result
  return 0

end Lentils.Unexpand
