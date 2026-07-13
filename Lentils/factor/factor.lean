/-
Factor — IO wrapper for the `factor` utility. 0BSD
-/

import Lentils.Common.IO.Native
import Lentils.factor.Logic

namespace Lentils.factor

open Logic
open Lentils.Common.IO.Native

/-- Extract whitespace-separated number tokens. -/
def tokens (s : String) : List String :=
  s.splitOn " " |>.filter (not ·.isEmpty) |>.flatMap (λ w => w.splitOn "\n")

def run (args : List String) : IO UInt32 := do
  let nums : List String :=
    if args.isEmpty then
      let input ← readStdinText
      tokens input
    else
      args
  let mut out := ""
  for t in nums do
    match t.trim.toNat? with
    | some n => out := out ++ formatFactorization n ++ "\n"
    | none => out := out ++ s!"{t}: error\n"
  IO.print out
  return 0

end Lentils.factor
