/-
Who — IO wrapper for the `who` utility. 0BSD

Lists logged-in users.
Uses Who.Logic (pure, verified) for formatting.
Uses FFI getutxent (same as `users`) for data collection.
-/

import Lentils.Who.Logic
import Lentils.Common.Errors

namespace Lentils.Who

open Logic
open Lentils.Common.Errors

/-- FFI: get list of who entries from utmpx.
    Returns an Array of strings, each formatted as "user|line|time_sec|host". -/
@[extern "lean_coreutils_who"]
opaque getWhoEntries : IO (Array String)

/--
Parse all raw entries and format them.
-/
def run (_args : List String) : IO UInt32 := do
  let rawEntries ← getWhoEntries
  let entries : List Entry :=
    rawEntries.toList.filterMap parseEntry
  if entries.isEmpty then
    return 0
  else
    let output := formatEntries entries
    IO.println output
    return 0

end Lentils.Who
