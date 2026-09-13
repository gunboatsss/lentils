/-
Cksum — IO wrapper for the `cksum` utility. 0BSD
-/

import Lentils.Common.IO.Native
import Lentils.Cksum.Logic

namespace Lentils.Cksum

open Logic
open Lentils.Common.IO.Native

def run (args : List String) : IO UInt32 := do
  -- Filter out `--help`-style flags are not supported; treat all
  -- non-flag operands as files (`-` means stdin, per GNU convention).
  let files := args.filter (λ a => a != "--help" && a != "-h")
  if args.contains "--help" || args.contains "-h" then
    printOut "Usage: cksum [FILE]...\nPrint CRC checksum and byte counts of each FILE.\n"
    return 0
  match files with
  | [] =>
    let input ← readStdin
    let result := format input
    printOut result
    printOut "\n"
    return 0
  | _ =>
    let mut failed := false
    for file in files do
      if file = "-" then
        let input ← readStdin
        let result := format input
        printOut result
        printOut "\n"
      else
        match (← try some <$> (do let f ← openFileRead file; readAll f) catch _ => pure none) with
        | some content =>
          let result := format content
          printOut (result ++ " " ++ file)
          printOut "\n"
        | none =>
          IO.eprintln s!"cksum: {file}: No such file or directory"
          failed := true
    return (if failed then 1 else 0)

end Lentils.Cksum
