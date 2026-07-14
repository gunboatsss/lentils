/-
Shuf — IO wrapper for the `shuf` utility. 0BSD -/
import Lentils.Common.IO.Native
import Lentils.Shuf.Logic

namespace Lentils.Shuf

open Logic
open Lentils.Common.IO.Native

def run (args : List String) : IO UInt32 := do
  let lines ←
    match args with
    | [] => readStdinLines
    | file :: _ => do
      try
        let content ← IO.FS.readFile file
        pure (content.splitOn "\n" |> List.filter (· ≠ ""))
      catch _ =>
        IO.eprintln s!"shuf: {file}: No such file or directory"
        pure []
  if lines.isEmpty then
    return 0
  let shuffled ← shuffle lines (λ n => IO.rand 0 n)
  for line in shuffled do
    IO.println line
  return 0

end Lentils.Shuf
