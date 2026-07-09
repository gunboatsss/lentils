/-
Sort — IO wrapper for the `sort` utility. 0BSD
-/

import Lentils.Common.Errors
import Lentils.Common.IO.Fd
import Lentils.Sort.Logic

namespace Lentils.Sort

open Lentils.Common.Errors
open Lentils.Common.IO.Fd
open Logic

partial def readAll (fd : UInt32) (bufSize : USize := 65536) : IO ByteArray := do
  let chunk ← readBytes fd bufSize
  if chunk.isEmpty then return ByteArray.empty
  else return chunk ++ (← readAll fd bufSize)

def tryWrite (buf : ByteArray) : IO Bool :=
  try let _ ← writeBytes 1 buf; return true catch _ => return false

def run (args : List String) : IO UInt32 := do
  ignoreSigpipe
  let opts := parseArgs args
  if args.contains "--help" || args.contains "-h" then
    let helpText := "Usage: sort [OPTION]... [FILE]...\nSort lines of text from FILE(s) or standard input.\n\nOptions:\n  -r, --reverse       reverse the result of comparisons\n  -n, --numeric-sort   compare according to string numerical value\n  -u                   output only the first of an equal run\n  -t CHAR              use CHAR as field separator\n  -k KEYDEF            sort by key (e.g., -k2, -k2n, -k2,2)\n      --help            display this help and exit\n"
    let _ ← writeBytes 1 helpText.toUTF8
    return 0
  let input ←
    match opts.filenames with
    | [] => readAll 0
    | file :: _ =>
      if file = "-" then readAll 0
      else
        let fdResult ← try
          let fd ← openFile file 0 0
          let content ← readAll fd
          closeFd fd
          pure content
        catch _ => pure ByteArray.empty
        pure fdResult
  let result := sortLines input opts
  let ok ← tryWrite result
  return if ok then 0 else 1

end Lentils.Sort
