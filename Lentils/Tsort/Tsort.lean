/-
Tsort — IO wrapper for the `tsort` utility. 0BSD -/
import Lentils.Common.IO.Native
import Lentils.Tsort.Logic

namespace Lentils.Tsort

open Logic
open Lentils.Common.IO.Native

def run (_args : List String) : IO UInt32 := do
  let lines ← readStdinLines
  let pairs := lines.filter (λ l => !l.isEmpty) |>.map (λ l =>
    match l.splitOn " " with
    | [a, b] => (a, b)
    | [a] => (a, a)
    | _ => ("", ""))
  let result := tsort pairs
  for node in result do
    IO.println node
  return 0

end Lentils.Tsort
