/-
Grep — IO wrapper for the `grep` utility. 0BSD
-/

import Lentils.Common.Errors
import Lentils.Common.IO.Fd
import Lentils.Grep.Logic

namespace Lentils.Grep

open Lentils.Common.Errors
open Lentils.Common.IO.Fd
open Logic

-- Reads all bytes from fd, appending a trailing newline if output is non-empty
-- so the last line doesn't run into the shell prompt.
partial def readAll (fd : UInt32) (bufSize : USize := 65536) : IO ByteArray := do
  let chunk ← readBytes fd bufSize
  if chunk.isEmpty then return ByteArray.empty
  else return chunk ++ (← readAll fd bufSize)

def run (args : List String) : IO UInt32 := do
  ignoreSigpipe
  let (flags, pattern, filenames) := parseArgs args
  if flags.showHelp then
    let helpText := "Usage: grep [OPTION]... PATTERN [FILE]...\nSearch for PATTERN in each FILE or standard input.\n\nOptions:\n  -i, --ignore-case     ignore case distinctions\n  -c, --count           print only a count of matching lines\n  -q, --quiet, --silent suppress all normal output\n  -v, --invert-match    select non-matching lines\n  -e, --regexp=PATTERN  use PATTERN as the pattern\n      --help            display this help and exit\n"
    let _ ← writeBytes 1 helpText.toUTF8
    return 0
  let input ←
    match filenames with
    | [] => readAll 0
    | file :: _ =>
      if file = "-" then readAll 0
      else
        match (← try
          let fd ← openFile file O_RDONLY 0
          let content ← readAll fd
          closeFd fd
          pure (some content)
        catch _ => pure none) with
        | some content => pure content
        | none =>
          -- POSIX: cannot open file, exit 2
          let _ ← writeBytes 2 (ByteArray.mk #[0x67, 0x72, 0x65, 0x70, 0x3a, 0x20]) -- "grep: "
          let _ ← writeBytes 2 file.toUTF8
          let _ ← writeBytes 2 (ByteArray.mk #[0x3a, 0x20, 0x4e, 0x6f, 0x20, 0x73, 0x75, 0x63, 0x68, 0x20, 0x66, 0x69, 0x6c, 0x65, 0x20, 0x6f, 0x72, 0x20, 0x64, 0x69, 0x72, 0x65, 0x63, 0x74, 0x6f, 0x72, 0x79, 0x0a])
          return 2
  let (result, hasMatch) := processInput input pattern flags
  if flags.quiet then
    if hasMatch then return 0 else return 1
  -- Add trailing newline so output doesn't run into the prompt
  let output := if result.isEmpty then result else result.push 0x0a
  let ok ← try
    let _ ← writeBytes 1 output
    pure true
  catch _ =>
    let _ ← writeBytes 2 (ByteArray.mk #[0x65, 0x72, 0x72, 0x0a])  -- "err\n"
    pure false
  -- POSIX: exit 0 if match found, 1 if no match, 2 if error
  if not ok then return 2
  else if not hasMatch then return 1
  else return 0

end Lentils.Grep
