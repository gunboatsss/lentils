/-
Grep — IO wrapper for the `grep` utility. 0BSD
-/

import Lentils.Common.Errors
import Lentils.Common.IO.Native
import Lentils.Grep.Logic

namespace Lentils.Grep

open Lentils.Common.Errors
open Lentils.Common.IO.Native
open Logic

def run (args : List String) : IO UInt32 := do
  ignoreSigpipe
  let (flags, pattern, filenames) := parseArgs args
  if flags.showHelp then
    let helpText := "Usage: grep [OPTION]... PATTERN [FILE]...\nSearch for PATTERN in each FILE or standard input.\n\nOptions:\n  -i, --ignore-case     ignore case distinctions\n  -c, --count           print only a count of matching lines\n  -q, --quiet, --silent suppress all normal output\n  -v, --invert-match    select non-matching lines\n  -e, --regexp=PATTERN  use PATTERN as the pattern\n      --help            display this help and exit\n"
    writeStdout helpText.toUTF8
    return 0
  let input ←
    match filenames with
    | [] => readStdin
    | file :: _ =>
      if file = "-" then readStdin
      else
        match (← try
          let f ← openFileRead file
          let content ← readAll f
          pure (some content)
        catch _ => pure none) with
        | some content => pure content
        | none =>
          writeStderr ("grep: " ++ file ++ ": No such file or directory\n").toUTF8
          return 2
  let (result, hasMatch) := processInput input pattern flags
  if flags.showFiles then
    if hasMatch then
      let fname := match filenames with
        | [] => "(standard input)"
        | f :: _ => if f = "-" then "(standard input)" else f
      writeStdout (fname.toUTF8.push 0x0a)
      return 0
    else
      return 1
  if flags.quiet then
    if hasMatch then return 0 else return 1
  -- Add trailing newline so output doesn't run into the prompt
  let output := if result.isEmpty then result else result.push 0x0a
  let ok ← try
    writeStdout output
    pure true
  catch _ =>
    writeStderr "err\n".toUTF8
    pure false
  -- POSIX: exit 0 if match found, 1 if no match, 2 if error
  if not ok then return 2
  else if not hasMatch then return 1
  else return 0

end Lentils.Grep
