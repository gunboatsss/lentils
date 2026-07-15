/-
Mkfifo — IO wrapper for the `mkfifo` utility.
0BSD

Creates FIFOs (named pipes) using the C FFI `mkfifo(3)`.

Provenance: POSIX.1-2017, Section "mkfifo — make FIFO special files".
No GPL source was consulted.
-/

import Lentils.Mkfifo.Logic
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Mkfifo

open Logic
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/--
Run the `mkfifo` utility.

Parses `[-m MODE] NAME...` and creates a FIFO for each name.
-/
def run (args : List String) : IO UInt32 := do
  let (opts, names) := parseArgs args
  if names.isEmpty then
    return ← exitUsage "mkfifo" "missing operand"
  let mut failed := false
  for n in names do
    try
      mkfifo n opts.mode
      if opts.verbose then
        IO.println s!"created fifo '{n}'"
    catch e =>
      IO.eprintln s!"mkfifo: cannot create fifo '{n}': {e.toString}"
      failed := true
  return if failed then 1 else 0

end Lentils.Mkfifo
