/-
Fmt — IO wrapper for the `fmt` utility. 0BSD
-/

import Lentils.Common.IO.Native
import Lentils.fmt.Logic

namespace Lentils.fmt

open Logic
open Lentils.Common.IO.Native

/-- Parse width from `-w N` / `-wN` style arguments. -/
def parseWidth (args : List String) : Nat :=
  let rec go (as : List String) (w : Nat) : Nat :=
    match as with
    | [] => w
    | a :: rest =>
      if a == "-w" then
        match rest with
        | n :: rest' => go rest' (n.toNat?.getD w)
        | [] => go [] w
      else if a.startsWith "-w" then
        go rest ((a.drop 2).toString.toNat?.getD w)
      else
        go rest w
  go args 75

def run (args : List String) : IO UInt32 := do
  let width := parseWidth args
  let input ← readStdinText
  let result := fmt input width
  IO.print result
  if !result.isEmpty then IO.print "\n"
  return 0

end Lentils.fmt
