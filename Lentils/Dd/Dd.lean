/-
Dd — IO wrapper for the `dd` utility. 0BSD
-/

import Lentils.Dd.Logic
import Lentils.Common.IO.Native

namespace Lentils.Dd

open Logic
open Lentils.Common.IO.Native

/-- Read all bytes from a file handle. -/
partial def readAllBytes (h : IO.FS.Handle) : IO ByteArray := do
  let chunk ← h.read (USize.ofNat 65536)
  if chunk.isEmpty then
    return ByteArray.empty
  else
    return chunk ++ (← readAllBytes h)

def run (args : List String) : IO UInt32 := do
  let mut ifile : Option String := none
  let mut ofile : Option String := none
  let mut statusLevel : String := "default"
  let ddArgs := args.filter (λ a => !a.startsWith "if=" && !a.startsWith "of=")
  for a in args do
    if a.startsWith "if=" then ifile := some (String.ofList (a.toList.drop 3))
    if a.startsWith "of=" then ofile := some (String.ofList (a.toList.drop 3))
    if a.startsWith "status=" then statusLevel := String.ofList (a.toList.drop 7)
  let params := parseArgs ddArgs
  let input : ByteArray ←
    match ifile with
    | some path =>
      try
        let h ← IO.FS.Handle.mk (System.FilePath.mk path) IO.FS.Mode.read
        readAllBytes h
      catch _ =>
        IO.eprintln s!"dd: {path}: No such file or directory"
        return (1 : UInt32)
    | none => readStdin
  let (output, result) := process input params
  match ofile with
  | some path =>
    try
      let h ← IO.FS.Handle.mk (System.FilePath.mk path) IO.FS.Mode.write
      h.write output
      h.flush
    catch _ =>
      IO.eprintln s!"dd: {path}: write error"
      return (1 : UInt32)
  | none => IO.print (String.fromUTF8! output)
  if statusLevel != "none" then
    IO.eprintln (formatSummary result)
  return (0 : UInt32)

end Lentils.Dd
