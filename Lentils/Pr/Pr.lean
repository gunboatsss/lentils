/-
Pr — IO wrapper for the `pr` utility. 0BSD
-/

import Lentils.Common.IO.Native
import Lentils.Pr.Logic

namespace Lentils.Pr

open Logic
open Lentils.Common.IO.Native

/-- Parse pagination options: -l, -w, -h, -t, and `-N` column counts. -/
def parseArgs (args : List String) : Nat × Nat × Nat × String × Bool :=
  let rec go (as : List String) (st : Nat × Nat × Nat × String × Bool) : Nat × Nat × Nat × String × Bool :=
    match as with
    | [] => st
    | a :: rest =>
      let (len, cols, width, title, showHeader) := st
      if a == "-t" then go rest (len, cols, width, title, false)
      else if a == "-h" then
        match rest with
        | t :: rest' => go rest' (len, cols, width, t, showHeader)
        | [] => go [] (len, cols, width, title, showHeader)
      else if a.startsWith "-h" then
        go rest (len, cols, width, (a.drop 2).toString, showHeader)
      else if a == "-l" then
        match rest with
        | n :: rest' => go rest' (n.toNat?.getD len, cols, width, title, showHeader)
        | [] => go [] (len, cols, width, title, showHeader)
      else if a.startsWith "-l" then
        go rest ((a.drop 2).toString.toNat?.getD len, cols, width, title, showHeader)
      else if a == "-w" then
        match rest with
        | n :: rest' => go rest' (len, cols, n.toNat?.getD width, title, showHeader)
        | [] => go [] (len, cols, width, title, showHeader)
      else if a.startsWith "-w" then
        go rest (len, cols, (a.drop 2).toString.toNat?.getD width, title, showHeader)
      else if !a.isEmpty && a.toList.all (·.isDigit) then
        go rest (len, a.toNat?.getD cols, width, title, showHeader)
      else
        go rest (len, cols, width, title, showHeader)
  go args (66, 1, 72, "(standard input)", true)

def run (args : List String) : IO UInt32 := do
  let (pageLength, columns, width, title, showHeader) := parseArgs args
  let input ← readStdinText
  let result := pr input pageLength columns width title showHeader
  IO.print result
  return 0

end Lentils.Pr
