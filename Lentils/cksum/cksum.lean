/-
Cksum — IO wrapper for the `cksum` utility. 0BSD
-/

import Lentils.Common.IO.Native
import Lentils.cksum.Logic

namespace Lentils.cksum

open Logic
open Lentils.Common.IO.Native

def run (_args : List String) : IO UInt32 := do
  let input ← readStdin
  let result := format input
  IO.print result
  IO.print "\n"
  return 0

end Lentils.cksum
