/-
Mknod — IO wrapper for the `mknod` utility.
0BSD

Creates block or character special files using the C FFI `mknod(2)`.

Provenance: POSIX.1-2017, Section "mknod — make block/character special files".
No GPL source was consulted.
-/

import Lentils.Mknod.Logic
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Mknod

open Logic
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/--
Run the `mknod` utility.

Parses `[-m MODE] NAME TYPE MAJOR MINOR` and creates the device node.
-/
def run (args : List String) : IO UInt32 := do
  match parseArgs args with
  | none => return ← exitUsage "mknod" "missing operand"
  | some (opts, name) =>
    let mode :=
      match opts.nodeType with
      | .block => opts.mode ||| 0o60000  -- S_IFBLK
      | .character => opts.mode ||| 0o20000  -- S_IFCHR
    try
      mknod name mode opts.major opts.minor
      if opts.verbose then
        let typeStr := if opts.nodeType = .block then "block" else "character"
        IO.println s!"created {typeStr} device '{name}' (major {opts.major}, minor {opts.minor})"
      return 0
    catch e =>
      IO.eprintln s!"mknod: cannot create '{name}': {e.toString}"
      return 1

end Lentils.Mknod
