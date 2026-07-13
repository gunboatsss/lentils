/-
Base64 — IO wrapper for the `base64` utility. 0BSD
-/

import Lentils.Base64.Logic
import Lentils.Common.IO.Native

namespace Lentils.Base64

open Logic
open Lentils.Common.IO.Native

/-- Run the base64 utility. -/
partial def run (args : List String) : IO UInt32 := do
  let decodeMode := args.any (λ a => a == "-d" || a == "--decode" || a == "-D")
  let wrap := !args.contains "-w0" && !args.contains "--wrap=0"
  let input ← readStdin
  if decodeMode then
    let inputStr := String.fromUTF8! input
    match decode inputStr with
    | some decoded =>
      IO.print (String.fromUTF8! decoded)
      return 0
    | none =>
      IO.eprintln "base64: invalid input"
      return 1
  else
    let encoded := encode input
    if wrap then
      let rec wrapLines (s : String) (acc : List String) : List String :=
        if s.length ≤ 76 then
          (s :: acc).reverse
        else
          let line := String.ofList (s.toList.take 76)
          let rest := String.ofList (s.toList.drop 76)
          wrapLines rest (line :: acc)
      let wrapped := String.intercalate "\n" (wrapLines encoded [])
      IO.println wrapped
    else
      IO.print encoded
    return 0

end Lentils.Base64
