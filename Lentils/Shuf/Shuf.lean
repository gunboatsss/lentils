/-
Shuf -- IO wrapper for the `shuf` utility. 0BSD

Implements Fisher-Yates shuffle with Lean's IO.rand.
Uses Shuf.Logic (pure, verified) for the permutation specification.
-/

import Lentils.Common.IO.Native
import Lentils.Shuf.Logic

namespace Lentils.Shuf

open Logic
open Lentils.Common.IO.Native

/--
Select the nth element from a list, returning it paired with the rest.
Uses structural recursion - no Array or index proofs needed.
-/
def takeNth : List String -> Nat -> String × List String
  | [], _ => ("", [])
  | x :: xs, 0 => (x, xs)
  | x :: xs, k+1 =>
    let (selected, rest) := takeNth xs k
    (selected, x :: rest)

/--
Recursively pick random elements from a list using IO.rand.
-/
partial def shuffleIO (items : List String) : IO (List String) :=
  match items with
  | [] => pure []
  | [x] => pure [x]
  | _ =>
    let n := items.length
    do
    let idx <- IO.rand 0 (n-1)
    let (selected, rest) := takeNth items idx
    let restShuffled <- shuffleIO rest
    pure (selected :: restShuffled)

/--
Run the `shuf` utility.
Reads lines from stdin or a file, shuffles them randomly, and prints to stdout.
Returns exit code 0.
-/
def run (args : List String) : IO UInt32 := do
  let lines <-
    match args with
    | [] => readStdinLines
    | file :: _ => do
      try
        let content <- IO.FS.readFile file
        pure (content.splitOn "\n" |> List.filter (fun x => x != ""))
      catch _ =>
        IO.eprintln s!"shuf: {file}: No such file or directory"
        pure []
  if lines.isEmpty then
    return 0
  let shuffled <- shuffleIO lines
  for line in shuffled do
    IO.println line
  return 0

end Lentils.Shuf
