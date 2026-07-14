/-
Fold — IO wrapper for the `fold` utility. 0BSD -/
import Lentils.Common.IO.Native
import Lentils.Fold.Logic

namespace Lentils.Fold

open Logic
open Lentils.Common.IO.Native

def run (args : List String) : IO UInt32 := do
  let width : Nat :=
    match args with
    | "-w" :: n :: _ => n.toNat?.getD 80
    | _ => 80
  if width = 0 then
    IO.eprintln "fold: invalid number of columns: \u20180\u2019: Numerical result out of range"
    return 1
  let breakSpaces := args.any (· = "-s")

  let input ← readStdinText

  let result := fold input width breakSpaces
  IO.print result
  return 0

end Lentils.Fold
