/-
Seq — IO wrapper for the `seq` utility. 0BSD -/
import Lentils.Seq.Logic

namespace Lentils.Seq

open Logic

def parseArgs (args : List String) : (Float × Float × Float) :=
  match args.filter (λ a => !a.startsWith "-") with
  | [] => (1.0, 1.0, 1.0)  -- error: no args
  | [last] => (1.0, parseFloat last, 1.0)
  | [first, last] => (parseFloat first, parseFloat last, 1.0)
  | [first, inc, last] => (parseFloat first, parseFloat last, parseFloat inc)
  | _ => (1.0, 1.0, 1.0)

def run (args : List String) : IO UInt32 := do
  let nonFlagArgs := args.filter (λ a => !a.startsWith "-")
  if nonFlagArgs.isEmpty then
    IO.eprintln "seq: missing operand"
    return 1
  let (first, last, inc) := parseArgs args
  if inc == 0.0 then
    IO.eprintln "seq: zero increment"
    return 1
  let nums := seq first last inc
  for n in nums do
    IO.println (formatFloat n)
  return 0

end Lentils.Seq
