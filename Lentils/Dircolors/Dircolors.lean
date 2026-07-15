/-
Dircolors — IO wrapper for the `dircolors` utility. 0BSD
-/

import Lentils.Dircolors.Logic
import Lentils.Common.IO.Native

namespace Lentils.Dircolors

open Logic
open Lentils.Common.IO.Native

def run (args : List String) : IO UInt32 := do
  let (opts, file) := parseArgs args

  if opts.printDatabase then
    for line in formatDatabase defaultDB do
      IO.println line
    return 0

  -- Determine the database to use
  let db : Database ←
    if file.isEmpty then
      pure defaultDB
    else
      try
        let content ← IO.FS.readFile file
        pure (parseDatabase content)
      catch _ =>
        pure defaultDB

  if opts.printLsColors then
    IO.print (entriesToLS_COLORS db)
    return 0

  -- Output shell commands to set LS_COLORS
  let lsColors := entriesToLS_COLORS db
  IO.print (formatShellOutput opts lsColors)
  return 0

end Lentils.Dircolors