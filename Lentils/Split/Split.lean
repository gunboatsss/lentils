/-
Split — IO wrapper for the `split` utility. 0BSD
-/

import Lentils.Split.Logic
import Lentils.Common.IO.Native

namespace Lentils.Split

open Logic
open Lentils.Common.IO.Native

structure SplitOpts where
  maxLines : Nat := 1000
  maxBytes : Nat := 0
  suffixLen : Nat := 2
  numericSuffix : Bool := false
  outPrefix : String := defaultPrefix
  modeLines : Bool := true

/-- Parse split options from args, returning options and remaining positional args. -/
def parseOpts (args : List String) : SplitOpts × List String :=
  let rec go (remaining : List String) (opts : SplitOpts) (files : List String) : SplitOpts × List String :=
    match remaining with
    | [] => (opts, files.reverse)
    | "-a" :: n :: rest =>
      match n.toNat? with
      | some v => go rest { opts with suffixLen := v } files
      | none => go rest opts files
    | "-b" :: n :: rest =>
      match parseSuffixed n with
      | some v => go rest { opts with maxBytes := v, modeLines := false } files
      | none => go rest opts files
    | "-l" :: n :: rest =>
      match n.toNat? with
      | some v => go rest { opts with maxLines := v, modeLines := true } files
      | none => go rest opts files
    | "-d" :: rest =>
      go rest { opts with numericSuffix := true } files
    | "--numeric-suffixes" :: rest =>
      go rest { opts with numericSuffix := true } files
    | "--" :: rest => (opts, files.reverse ++ rest)
    | f :: rest => if f.startsWith "-" then go rest opts files else go rest opts (f :: files)
  go args {} []

/-- Read all bytes from a file. -/
partial def readFileBytes (path : String) : IO ByteArray := do
  let h ← IO.FS.Handle.mk (System.FilePath.mk path) IO.FS.Mode.read
  let rec go (acc : ByteArray) : IO ByteArray := do
    let chunk ← h.read (USize.ofNat 65536)
    if chunk.isEmpty then pure acc
    else go (acc ++ chunk)
  go ByteArray.empty

def run (args : List String) : IO UInt32 := do
  let (opts, files) := parseOpts args
  if opts.modeLines then
    let input ←
      match files with
      | [] => readStdinText
      | [f] => try IO.FS.readFile (System.FilePath.mk f) catch _ => pure ""
      | _ => readStdinText
    let chunks := splitByLines input opts.maxLines opts.suffixLen opts.numericSuffix
    for (sfx, content) in chunks do
      let filename := opts.outPrefix ++ sfx
      try
        IO.FS.writeFile (System.FilePath.mk filename) content
      catch _ =>
        IO.eprintln s!"split: {filename}: write error"
        return (1 : UInt32)
    return (0 : UInt32)
  else
    let input ←
      match files with
      | [] => readStdin
      | [f] =>
        try readFileBytes f
        catch _ => pure ByteArray.empty
      | _ => readStdin
    let chunks := splitBytes input opts.maxBytes opts.suffixLen opts.numericSuffix
    for (sfx, content) in chunks do
      let filename := opts.outPrefix ++ sfx
      try
        let h ← IO.FS.Handle.mk (System.FilePath.mk filename) IO.FS.Mode.write
        h.write content
        h.flush
      catch _ =>
        IO.eprintln s!"split: {filename}: write error"
        return (1 : UInt32)
    return (0 : UInt32)

end Lentils.Split
