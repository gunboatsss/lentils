/-
Md5sum — IO wrapper for the `md5sum` utility. 0BSD
-/

import Lentils.Common.IO.Native
import Lentils.Md5sum.Logic

namespace Lentils.Md5sum

open Logic
open Lentils.Common.IO.Native

/--
Run the md5sum utility.
With no arguments, reads stdin. Otherwise, reads each file and prints hash + filename.
-/
def run (_args : List String) : IO UInt32 := do
  let input ← readStdin
  IO.print (formatStdin input)
  return 0

end Lentils.Md5sum
