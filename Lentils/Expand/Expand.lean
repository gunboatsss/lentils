/-
Expand — IO wrapper for the `expand` utility. 0BSD -/
import Lentils.Common.IO.Native
import Lentils.Expand.Logic

namespace Lentils.Expand

open Logic
open Lentils.Common.IO.Native

def run (args : List String) : IO UInt32 := do
  let tabSize : Nat :=
    match args with
    | "-t" :: n :: _ => n.toNat?.getD 8
    | _ => 8
  let input ← readStdinText
  let result := expand input tabSize
  IO.print result
  return 0

end Lentils.Expand
