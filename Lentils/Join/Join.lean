/-
Join — IO wrapper for the `join` utility. 0BSD -/
import Lentils.Join.Logic

namespace Lentils.Join

open Logic

def run (args : List String) : IO UInt32 := do
  let files := args.filter (λ a => !a.startsWith "-")
  let lines1 ←
    match files with
    | f1 :: _ => do
      try
        let c ← IO.FS.readFile f1
        pure (c.splitOn "\n")
      catch _ => pure []
    | [] => pure []
  let lines2 ←
    match files with
    | _ :: f2 :: _ => do
      try
        let c ← IO.FS.readFile f2
        pure (c.splitOn "\n")
      catch _ => pure []
    | _ => pure []
  let result := join lines1 lines2
  IO.print result
  return 0

end Lentils.Join
