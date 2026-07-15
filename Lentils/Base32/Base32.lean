/-
Base32 — IO wrapper for the `base32` utility. 0BSD
-/

import Lentils.Base32.Logic
import Lentils.Common.IO.Native

namespace Lentils.Base32

open Logic
open Lentils.Common.IO.Native

/-- Read file content as ByteArray. Returns `none` on error. -/
def readFileBytes (path : String) : IO (Option ByteArray) := do
  try
    let f ← openFileRead path
    let content ← readAll f
    return some content
  catch _ =>
    return none

/-- Get non-flag operands from args. -/
def getFiles (args : List String) : List String :=
  let rec go (remaining : List String) (acc : List String) : List String :=
    match remaining with
    | [] => acc.reverse
    | "--" :: rest => acc.reverse ++ rest
    | s :: rest =>
      if s.startsWith "-" then go rest acc
      else go rest (s :: acc)
  go args []

def run (args : List String) : IO UInt32 := do
  let decodeMode := args.any (λ a => a == "-d" || a == "--decode")
  let files := getFiles args

  if decodeMode then
    if files.isEmpty then
      let input ← readStdin
      let inputStr := String.fromUTF8! input
      match decode inputStr with
      | some decoded =>
        IO.print (String.fromUTF8! decoded)
        return 0
      | none =>
        IO.eprintln "base32: invalid input"
        return 1
    else
      let mut failed := false
      for file in files do
        match ← readFileBytes file with
        | none =>
          IO.eprintln s!"base32: {file}: error reading file"
          failed := true
        | some content =>
          let inputStr := String.fromUTF8! content
          match decode inputStr with
          | some decoded =>
            IO.print (String.fromUTF8! decoded)
          | none =>
            IO.eprintln s!"base32: {file}: invalid input"
            failed := true
      return if failed then 1 else 0
  else
    if files.isEmpty then
      let input ← readStdin
      IO.println (encode input)
      return 0
    else
      let mut failed := false
      for file in files do
        match ← readFileBytes file with
        | none =>
          IO.eprintln s!"base32: {file}: error reading file"
          failed := true
        | some content =>
          IO.println (encode content)
      return if failed then 1 else 0

end Lentils.Base32
