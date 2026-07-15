/-
Basenc — IO wrapper for the `basenc` utility.
0BSD

Generic base encoding utility supporting base64, base32, and base16.
Delegates to the existing base32/base64 Logic modules for those encodings.

Provenance: GNU coreutils `basenc`.
No GPL source was consulted.
-/

import Lentils.Basenc.Logic
import Lentils.Base64.Logic
import Lentils.Base32.Logic
import Lentils.Common.IO.Native

namespace Lentils.Basenc

open Logic
open Lentils.Common.IO.Native

/-- Read file content. Returns empty on error. -/
def readFileBytes (path : String) : IO ByteArray := do
  try
    let f ← openFileRead path
    readAll f
  catch _ => pure ByteArray.empty

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

/-- Encode a ByteArray using the selected encoding. -/
def encodeWith (enc : Encoding) (data : ByteArray) : String :=
  match enc with
  | .base64 => Lentils.Base64.Logic.encode data
  | .base32 => Lentils.Base32.Logic.encode data
  | .base16 => encodeBase16 data

/-- Decode a string using the selected encoding. Returns none on failure. -/
def decodeWith (enc : Encoding) (s : String) : Option ByteArray :=
  match enc with
  | .base64 => Lentils.Base64.Logic.decode s
  | .base32 => Lentils.Base32.Logic.decode s
  | .base16 => decodeBase16 s

def run (args : List String) : IO UInt32 := do
  let (opts, rawFiles) := parseArgs args
  let files := if rawFiles.isEmpty then [] else rawFiles

  if opts.decode then
    if files.isEmpty then
      let input ← readStdin
      let inputStr := String.fromUTF8! input
      match decodeWith opts.encoding inputStr with
      | some decoded =>
        IO.print (String.fromUTF8! decoded)
        return 0
      | none =>
        IO.eprintln "basenc: invalid input"
        return 1
    else
      let mut failed := false
      for file in files do
        let content ← readFileBytes file
        let inputStr := String.fromUTF8! content
        match decodeWith opts.encoding inputStr with
        | some decoded =>
          IO.print (String.fromUTF8! decoded)
        | none =>
          IO.eprintln s!"basenc: {file}: invalid input"
          failed := true
      return if failed then 1 else 0
  else
    if files.isEmpty then
      let input ← readStdin
      let encoded := encodeWith opts.encoding input
      IO.println encoded
      return 0
    else
      let mut failed := false
      for file in files do
        let content ← readFileBytes file
        let encoded := encodeWith opts.encoding content
        IO.println encoded
      return if failed then 1 else 0

end Lentils.Basenc
