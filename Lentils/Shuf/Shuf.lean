/-
Shuf — IO wrapper for the `shuf` utility. 0BSD -/
import Lentils.Shuf.Logic

namespace Lentils.Shuf

open Logic

def run (args : List String) : IO UInt32 := do
  let input ←
    try IO.FS.readFile "/dev/stdin"
    catch _ => pure ""
  let lines := input.splitOn "\n"
  -- Remove empty trailing line if present (input typically ends with \n)
  let lines := if lines.length > 0 then
    let lastIdx := lines.length - 1
    match lines.drop lastIdx with
    | [""] => lines.take lastIdx
    | _ => lines
  else lines
  let shuffled ← shuffle lines (λ n => IO.rand 0 n)
  for line in shuffled do
    IO.println line
  return 0

end Lentils.Shuf
