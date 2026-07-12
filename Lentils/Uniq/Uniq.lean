/-
Uniq — IO wrapper for the `uniq` utility.
0BSD
-/

import Lentils.Common.Errors
import Lentils.Common.IO.Native
import Lentils.Uniq.Logic

namespace Lentils.Uniq

open Lentils.Common.Errors
open Lentils.Common.IO.Native
open Logic

def run (args : List String) : IO UInt32 := do
  ignoreSigpipe
  let (mode, filenames) := parseArgs args
  let input ←
    match filenames with
    | [] => readStdin
    | file :: _ =>
      if file = "-" then readStdin
      else
        try
          let f ← openFileRead file
          readAll f
        catch _ =>
          pure ByteArray.empty
  let result := processLines input mode
  try
    writeStdout result
    return 0
  catch _ =>
    return 1

end Lentils.Uniq
