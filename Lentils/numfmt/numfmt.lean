/-
Numfmt — IO wrapper for the `numfmt` utility. 0BSD
-/

import Lentils.Common.IO.Native
import Lentils.numfmt.Logic

namespace Lentils.numfmt

open Logic
open Lentils.Common.IO.Native

/-- Parse `--to` / `--from` / `--to-unit` options. -/
def parseOpts (args : List String) : Mode × Option Nat :=
  let rec go (as : List String) (mode : Mode) (toUnit : Option Nat) : Mode × Option Nat :=
    match as with
    | [] => (mode, toUnit)
    | a :: rest =>
      if a == "--to=si" then go rest Mode.toSI toUnit
      else if a == "--to=iec" || a == "--to=iec-i" then go rest Mode.toIEC toUnit
      else if a == "--to=none" then go rest Mode.passthrough toUnit
      else if a == "--from=si" then go rest Mode.fromSI toUnit
      else if a == "--from=iec" || a == "--from=iec-i" then go rest Mode.fromIEC toUnit
      else if a.startsWith "--to-unit=" then
        let u := (a.drop 10).toString
        match u.toList with
        | [c] => go rest mode (letterExp c)
        | _ => go rest mode toUnit
      else go rest mode toUnit
  go args Mode.passthrough none

def run (args : List String) : IO UInt32 := do
  let (mode, toUnit) := parseOpts args
  let input ← readStdinText
  let result := numfmt input mode toUnit
  IO.print result
  if !result.isEmpty then IO.print "\n"
  return 0

end Lentils.numfmt
