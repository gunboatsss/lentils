/-
Who — IO wrapper for the `who` utility. 0BSD

Lists logged-in users with optional -T (state) and -u (idle/PID) flags.
Uses Who.Logic for formatting.
-/

import Lentils.Who.Logic

namespace Lentils.Who

open Logic

/-- FFI: get list of who entries from utmpx.
    Returns an Array of strings, each formatted as
    "user|line|time_str|host|state|idle_secs|pid". -/
@[extern "lean_coreutils_who"]
opaque getWhoEntries : IO (Array String)

/--
Parse all raw entries and format them.
Supports -T (show state character) and -u (show idle/PID).
-/
def run (args : List String) : IO UInt32 := do
  let showState := args.any (· = "-T")
  let showIdle := args.any (· = "-u")
  
  let rawEntries ← getWhoEntries
  let entries : List Entry :=
    rawEntries.toList.filterMap parseEntry |>.reverse
  if entries.isEmpty then
    return 0
  else
    let output := formatEntries entries showState showIdle
    IO.println output
    return 0

end Lentils.Who
