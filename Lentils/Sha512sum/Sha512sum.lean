/-
Sha512sum — IO wrapper for the `sha512sum` utility. 0BSD
-/

import Lentils.Common.IO.Native
import Lentils.Sha512sum.Logic

namespace Lentils.Sha512sum

open Logic
open Lentils.Common.IO.Native

/--
Run the sha512sum utility.
With no arguments, reads stdin. Otherwise, reads each file and prints hash + filename.
-/
def run (_args : List String) : IO UInt32 := do
  let input ← readStdin
  IO.print (formatStdin input)
  return 0

end Lentils.Sha512sum
