/-
Cut — IO wrapper for the `cut` utility. 0BSD
-/

import Lentils.Common.Errors
import Lentils.Common.IO.Native
import Lentils.Cut.Logic

namespace Lentils.Cut

open Lentils.Common.Errors
open Lentils.Common.IO.Native
open Logic

def run (args : List String) : IO UInt32 := do
  ignoreSigpipe
  let cfg := parseArgs args
  let input ← readStdin
  let result := processInput input cfg
  try
    writeStdout result
    return 0
  catch _ =>
    return 1

end Lentils.Cut
