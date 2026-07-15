/-
Mktemp — IO wrapper for the `mktemp` utility.
0BSD

Creates temporary files or directories using `mkstemp(3)` / `mkdtemp(3)`.

Provenance: POSIX.1-2017, Section "mktemp — create temporary files/dirs".
No GPL source was consulted.
-/

import Lentils.Mktemp.Logic
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Mktemp

open Logic
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/--
Run the `mktemp` utility.

Parses options and template, creates the temp file/dir, and prints its name.
-/
def run (args : List String) : IO UInt32 := do
  let (opts, userTemplate) := parseArgs args
  let template := buildTemplate opts userTemplate

  -- Check template has enough X's
  if !template.contains "X" then
    IO.eprintln s!"mktemp: template '{template}' must contain at least one 'X'"
    return 1

  -- Check template has at least 3 X's (POSIX minimum is 6, but be lenient)
  let xCount := template.toList.filter (λ c => c = 'X') |>.length
  if xCount < 3 then
    IO.eprintln s!"mktemp: too few X's in template '{template}'"
    return 1

  try
    if opts.dryRun then
      -- -u: don't create, just print what would be created
      IO.println template
      return 0
    else if opts.directory then
      let name ← mkdtemp template
      IO.println name
      return 0
    else
      let name ← mkstemp template
      IO.println name
      return 0
  catch e =>
    IO.eprintln s!"mktemp: {e.toString}"
    return 1

end Lentils.Mktemp
