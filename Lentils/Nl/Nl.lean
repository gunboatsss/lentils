/-
Nl — IO wrapper for the `nl` utility. 0BSD -/
import Lentils.Common.IO.Native
import Lentils.Nl.Logic

namespace Lentils.Nl

open Logic
open Lentils.Common.IO.Native

def run (_args : List String) : IO UInt32 := do
  let input ← readStdinText
  let result := numberLines input
  if !result.isEmpty then
    IO.print result
    IO.print "\n"
  return 0

end Lentils.Nl
