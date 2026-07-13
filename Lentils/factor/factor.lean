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
  s.splitOn " " |>.filter (·.isEmpty == false) |>.flatMap (λ w => w.splitOn "\n" |>.filter (·.isEmpty == false))

def run (args : List String) : IO UInt32 := do
  let input ←
    if args.isEmpty then
      readStdinText
    else
      pure ""
  let nums : List String :=
    if args.isEmpty then
      tokens input
    else
      args
  let mut out := ""
  let mut exitCode : UInt32 := 0
  for t in nums do
    match t.trimAscii.toNat? with
    | some n => out := out ++ formatFactorization n ++ "\n"
    | none => do
      IO.eprintln s!"{t}: error"
      exitCode := 1
  IO.print out
  return exitCode

end Lentils.factor
