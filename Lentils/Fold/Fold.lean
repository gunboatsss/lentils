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
  let _breakSpaces := args.any (· = "-s")  -- TODO: wire -s into fold logic

  let input ← readStdinText

  let result := fold input width
  IO.print result
  return 0

end Lentils.Fold
