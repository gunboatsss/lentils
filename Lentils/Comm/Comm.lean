/-
Comm — IO wrapper for the `comm` utility. 0BSD -/
import Lentils.Comm.Logic

namespace Lentils.Comm

open Logic

def parseFlags (args : List String) : SuppressFlags :=
  let col1 := args.any (· = "-1")
  let col2 := args.any (· = "-2")
  let col3 := args.any (· = "-3")
  { col1, col2, col3 }

def readFileLines (path : String) : IO (List String) := do
  let content ←
    try IO.FS.readFile path
    catch _ => pure ""
  pure (content.splitOn "\n")

def run (args : List String) : IO UInt32 := do
  let flags := parseFlags args
  let files := args.filter (λ a => !a.startsWith "-")
  let lines1 ←
    match files with
    | f1 :: _ => readFileLines f1
    | [] => pure []
  let lines2 ←
    match files with
    | _ :: f2 :: _ => readFileLines f2
    | _ => pure []
  let result := comm lines1 lines2 flags
  IO.print result
  return 0

end Lentils.Comm
