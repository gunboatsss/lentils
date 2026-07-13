/-
Shuf — IO wrapper for the `shuf` utility. 0BSD -/
import Lentils.Common.IO.Native
import Lentils.Shuf.Logic

namespace Lentils.Shuf

open Logic
open Lentils.Common.IO.Native

def run (_args : List String) : IO UInt32 := do
  let lines ← readStdinLines
  let shuffled ← shuffle lines (λ n => IO.rand 0 n)
  for line in shuffled do
    IO.println line
  return 0

end Lentils.Shuf
