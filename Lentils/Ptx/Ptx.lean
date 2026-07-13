/-
Ptx — IO wrapper for the `ptx` utility. 0BSD
-/

import Lentils.Ptx.Logic
import Lentils.Common.IO.Native

namespace Lentils.Ptx

open Logic
open Lentils.Common.IO.Native

structure PtxOpts where
  gFlag : Bool := false
  foldCase : Bool := false
  files : List String := []

/-- Parse ptx options from args. -/
def parseOpts (args : List String) : PtxOpts :=
  let rec go (remaining : List String) (opts : PtxOpts) : PtxOpts :=
    match remaining with
    | [] => opts
    | "-g" :: rest => go rest { opts with gFlag := true }
    | "-f" :: rest => go rest { opts with foldCase := true }
    | "--" :: rest => { opts with files := opts.files.reverse ++ rest }
    | f :: rest => if f.startsWith "-" then go rest opts else go rest { opts with files := f :: opts.files }
  go args {}

def run (args : List String) : IO UInt32 := do
  let opts := parseOpts args
  let input ←
    match opts.files with
    | [] => readStdinText
    | fs =>
      let rec readFiles (remaining : List String) (acc : String) : IO String :=
        match remaining with
        | [] => pure acc
        | f :: rest =>
          try
            let content ← IO.FS.readFile (System.FilePath.mk f)
            readFiles rest (acc ++ content ++ "\n")
          catch _ =>
            IO.eprintln s!"ptx: {f}: No such file or directory"
            return acc
      readFiles fs ""
  let output := generate input opts.gFlag opts.foldCase
  if output.isEmpty then
    pure ()
  else
    IO.println output
  return 0

end Lentils.Ptx
