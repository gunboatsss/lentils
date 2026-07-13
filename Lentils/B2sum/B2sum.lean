/-
B2sum — IO wrapper for the `b2sum` utility. 0BSD
-/

import Lentils.Common.IO.Native
import Lentils.B2sum.Logic

namespace Lentils.B2sum

open Logic
open Lentils.Common.IO.Native

/--
Run the b2sum utility.
With no arguments, reads stdin. Otherwise, reads each file and prints hash + filename.
-/
def run (_args : List String) : IO UInt32 := do
  let input ← readStdin
  IO.print (formatStdin input)
  return 0

end Lentils.B2sum
