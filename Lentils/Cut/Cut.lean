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
  if cfg.invalid then
    IO.eprintln "cut: invalid field/byte list"
    IO.eprintln "Try 'cut --help' for more information."
    return 1
  let mut failed := false
  let input ←
    match cfg.filenames with
    | [] => readStdin
    | files =>
      let mut acc := ByteArray.empty
      for file in files do
        if file = "-" then
          acc := acc ++ (← readStdin)
        else
          match (← try some <$> (do let f ← openFileRead file; readAll f) catch _ => pure none) with
          | some content => acc := acc ++ content
          | none =>
            IO.eprintln s!"cut: {file}: No such file or directory"
            failed := true
      pure acc
  let result := processInput input cfg
  try
    writeStdout result
    return (if failed then 1 else 0)
  catch _ =>
    return 1

end Lentils.Cut
