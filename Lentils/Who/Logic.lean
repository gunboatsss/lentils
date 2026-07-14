/-
Who.Logic — Pure who-output formatting logic for `who`. 0BSD

Contains ONLY pure functions: parsing raw entries from FFI,
formatting the who table, and related logic.
No IO is performed here. All FFI lives in Who.lean.
-/

namespace Lentils.Who.Logic

/--
A single who entry parsed from the raw FFI string.
The raw format is "user|line|time_sec|host".
-/
structure Entry where
  user : String
  line : String
  timeSec : String  -- epoch seconds as decimal string
  host : String
  deriving Repr, BEq, DecidableEq

/--
Parse a raw pipe-delimited entry string into an Entry.
Returns none if the string is malformed.
Uses splitOn which returns List String.
-/
def parseEntry (raw : String) : Option Entry :=
  let parts := raw.splitOn "|"
  match parts with
  | [user, line, timeSec] =>
    some { user, line, timeSec, host := "" }
  | [user, line, timeSec, host] =>
    some { user, line, timeSec, host }
  | _ => none

/--
Right-pad a string to a given width with spaces.
-/
def padRight (s : String) (w : Nat) : String :=
  let slen := s.length
  if slen >= w then
    (s.take w).toString
  else
    s ++ String.ofList (List.replicate (w - slen) ' ')

/--
Format a timestamp string (already formatted by C code as YYYY-MM-DD HH:MM).
-/
def formatTime (timeStr : String) : String :=
  if timeStr.isEmpty then "?" else timeStr

/--
Format an Entry in the GNU who default output format:
  USER      LINE         TIME                 HOST
Uses fixed-width columns similar to GNU coreutils who.
-/
def formatEntry (e : Entry) : String :=
  let userPadded := padRight e.user 8
  let linePadded := padRight e.line 8
  let time := formatTime e.timeSec
  -- GNU who uses format: USER LINE DATE HOST
  -- The time field is right-aligned with spaces; host gets one space prefix
  let hostPart := if e.host.isEmpty then "" else s!" ({e.host})"
  s!"{userPadded} {linePadded}     {time}{hostPart}"

/--
Format a list of entries as the complete who output.
Each entry is on its own line (no trailing newline for the last entry).
-/
def formatEntries (entries : List Entry) : String :=
  String.intercalate "\n" (entries.map formatEntry)

/--
Count the number of logged-in sessions.
-/
def countSessions (entries : List Entry) : Nat :=
  entries.length

/--
Check if a username appears among the entries.
-/
def hasUser (entries : List Entry) (username : String) : Bool :=
  entries.any (λ e => e.user = username)

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- Empty entries produce empty output. -/
example : formatEntries [] = "" := by
  native_decide

/-- Parsing a well-formed entry. -/
example : parseEntry "root|console|1718200000|myhost" =
  some { user := "root", line := "console", timeSec := "1718200000", host := "myhost" } := by
  native_decide

/-- Parsing an entry without host. -/
example : parseEntry "jane|pts/0|1718201000" =
  some { user := "jane", line := "pts/0", timeSec := "1718201000", host := "" } := by
  native_decide

/-- Parsing a malformed entry returns none. -/
example : parseEntry "invalid|entry" = none := by
  native_decide

/-- Counting sessions. -/
example : countSessions [{ user := "root", line := "console", timeSec := "1000", host := "" }] = 1 := by
  native_decide

/-- Empty session count. -/
example : countSessions [] = 0 := by
  native_decide

/-- hasUser finds existing user. -/
example : hasUser [{ user := "root", line := "console", timeSec := "1000", host := "" }] "root" = true := by
  native_decide

/-- hasUser returns false for missing user. -/
example : hasUser [{ user := "root", line := "console", timeSec := "1000", host := "" }] "jane" = false := by
  native_decide

end Lentils.Who.Logic
