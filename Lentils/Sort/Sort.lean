/-
Sort — IO wrapper for the `sort` utility. 0BSD
-/

import Lentils.Common.Errors
import Lentils.Common.IO.Native
import Lentils.Sort.Logic

namespace Lentils.Sort

open Lentils.Common.Errors
open Lentils.Common.IO.Native
open Logic

def run (args : List String) : IO UInt32 := do
  ignoreSigpipe
  let opts := parseArgs args
  if args.contains "--help" || args.contains "-h" then
    let helpText := "Usage: sort [OPTION]... [FILE]...\nSort lines of text from FILE(s) or standard input.\n\nOptions:\n  -r, --reverse       reverse the result of comparisons\n  -n, --numeric-sort   compare according to string numerical value\n  -u                   output only the first of an equal run\n  -t CHAR              use CHAR as field separator\n  -k KEYDEF            sort by key (e.g., -k2, -k2n, -k2,2)\n      --help            display this help and exit\n"
    writeStdout helpText.toUTF8
    return 0
  let input ←
    match opts.filenames with
    | [] => readStdin
    | file :: _ =>
      if file = "-" then readStdin
      else
        try
          let f ← openFileRead file
          readAll f
        catch _ => pure ByteArray.empty
  let result := sortLines input opts
  try
    writeStdout result
    return 0
  catch _ =>
    return 1

end Lentils.Sort
