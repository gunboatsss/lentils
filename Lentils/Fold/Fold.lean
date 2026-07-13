/-
Fold — IO wrapper for the `fold` utility. 0BSD -/
import Lentils.Fold.Logic

namespace Lentils.Fold

open Logic

def run (args : List String) : IO UInt32 := do
  let width : Nat :=
    match args with
    | "-w" :: n :: _ => n.toNat?.getD 80
    | _ => 80
  let breakSpaces := args.any (· = "-s")

  let input ←
    try IO.FS.readFile "/dev/stdin"
    catch _ => pure ""

  let result := fold input width
  IO.print result
  return 0

end Lentils.Fold
