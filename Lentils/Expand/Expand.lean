/-
Expand — IO wrapper for the `expand` utility. 0BSD -/
import Lentils.Expand.Logic

namespace Lentils.Expand

open Logic

def run (args : List String) : IO UInt32 := do
  -- Parse -t flag for tab size
  let tabSize : Nat :=
    match args with
    | "-t" :: n :: _ => n.toNat?.getD 8
    | _ => 8

  -- Read stdin as a string
  let input ←
    try IO.FS.readFile "/dev/stdin"
    catch _ =>
      pure ""

  let result := expand input tabSize
  IO.print result
  return 0

end Lentils.Expand
