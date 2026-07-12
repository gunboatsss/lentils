/-
Tr — IO wrapper for the `tr` utility. 0BSD
-/

import Lentils.Common.Errors
import Lentils.Common.IO.Native
import Lentils.Tr.Logic

namespace Lentils.Tr

open Lentils.Common.Errors
open Lentils.Common.IO.Native
open Logic

def run (args : List String) : IO UInt32 := do
  ignoreSigpipe
  let (mode, complement, set1, set2) := parseArgs args
  let input ← readStdin
  let result := processInput input mode complement set1 set2
  try
    writeStdout result
    return 0
  catch _ =>
    return 1

end Lentils.Tr
