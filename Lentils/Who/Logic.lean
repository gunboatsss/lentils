/-
Who.Logic — Pure who-output formatting logic for `who`. 0BSD

Contains ONLY pure functions: parsing raw entries from FFI,
formatting the who table, and related logic.
-/

namespace Lentils.Who.Logic

/--
A single who entry parsed from the raw FFI string.
The raw format is "user|line|time_str|host|state|idle_secs|pid".
-/
structure Entry where
  user : String
  line : String
  timeStr : String   -- "YYYY-MM-DD HH:MM"
  host : String
  state : String     -- "+", "-", or "?"
  idleSecs : String  -- decimal seconds as string
  pid : String       -- decimal PID as string
  deriving Repr, DecidableEq, BEq

/--
Parse a raw pipe-delimited entry string into an Entry.
-/
def parseEntry (raw : String) : Option Entry :=
  let parts := raw.splitOn "|"
  match parts with
  | [user, line, timeStr, host, state, idleSecs, pid] =>
    some { user, line, timeStr, host, state, idleSecs, pid }
  | _ => none

/--
Right-pad a string to a given width. Truncates if longer.
-/
def padRight (s : String) (w : Nat) : String :=
  let chars := s.toList
  if chars.length ≥ w then String.ofList (chars.take w)
  else s ++ String.ofList (List.replicate (w - chars.length) ' ')

/--
Left-pad a string to a given width (right-justified). Truncates if longer.
-/
def padLeft (s : String) (w : Nat) : String :=
  let chars := s.toList
  if chars.length ≥ w then String.ofList (chars.take w)
  else String.ofList (List.replicate (w - chars.length) ' ') ++ s

/--
Format idle seconds into a human-readable idle string.
-/
def formatIdle (secsStr : String) : String :=
  match secsStr.toNat? with
  | none => "?"
  | some secs =>
    if secs < 60 then "."
    else if secs < 3600 then
      let m := secs / 60
      let mStr := if m < 10 then "0" ++ toString m else toString m
      s!"00:{mStr}"
    else if secs < 86400 then
      let h := secs / 3600
      let m := (secs % 3600) / 60
      let hStr := if h < 10 then "0" ++ toString h else toString h
      let mStr := if m < 10 then "0" ++ toString m else toString m
      s!"{hStr}:{mStr}"
    else
      let days := secs / 86400
      s!"{toString days}day"

/--
Format a single Entry matching GNU who output.

Fields:
  (1) %-8s  username
  (2) %s    state, with a space before it (only with -T)
  (3) %-12s terminal, with a space before it
  (4) %-*s  time, with a space before it (width = max across entries)
  (5) %s    idle in 6 chars right-justified, with space before (only with -u)
  (6) %s    pid in 10 chars right-justified, with space before (only with -u)
  (7) host  displayed as (host), no trailing padding

No trailing whitespace because the last field is unpadded.
-/
def formatEntry (e : Entry) (timeWidth : Nat) (showState : Bool) (showIdle : Bool) : String :=
  let userPart := padRight e.user 8
  let statePart := if showState then s!" {e.state}" else ""
  let linePart := s!" {padRight e.line 12}"
  let timePart := s!" {padRight e.timeStr timeWidth}"
  let idlePart :=
    if showIdle then
      -- If state is '?' (not a tty), idle shows '?' too (matching GNU who -u)
      let idleStr := if e.state = "?" then "?" else formatIdle e.idleSecs
      s!" {padLeft idleStr 5}"
    else ""
  let pidPart :=
    if showIdle then
      s!" {padLeft e.pid 9}"
    else ""
  -- Host: wrap in parens like GNU who
  let hostPart := if e.host.isEmpty then "" else s!" ({e.host})"
  s!"{userPart}{statePart}{linePart}{timePart}{idlePart}{pidPart}{hostPart}"

/--
Format a list of entries as the complete who output.
-/
def formatEntries (entries : List Entry) (showState : Bool := false) (showIdle : Bool := false) : String :=
  let maxTimeWidth := entries.foldl (λ m e =>
    max m e.timeStr.length) 0
  let formatted := entries.map (λ e => formatEntry e maxTimeWidth showState showIdle)
  String.intercalate "\n" formatted

-- ─── Helpers ──────────────────────────────────────────────────────────────────

/-- Show state: whether to include the state character column. -/
def showState (args : List String) : Bool :=
  args.any (· = "-T")

/-- Show idle: whether to include the idle time and PID columns. -/
def showIdle (args : List String) : Bool :=
  args.any (· = "-u")

/-- Count the number of logged-in sessions. -/
def countSessions (entries : List Entry) : Nat :=
  entries.length

/-- Check if a username appears among the entries. -/
def hasUser (entries : List Entry) (username : String) : Bool :=
  entries.any (λ e => e.user = username)

-- ─── Theorems ────────────────────────────────────────────────────────────────

/-- Parsing a well-formed entry with all fields. -/
example : parseEntry "root|console|2026-07-14 22:39|myhost|+|3600|1234" =
  some { user := "root", line := "console", timeStr := "2026-07-14 22:39",
         host := "myhost", state := "+", idleSecs := "3600", pid := "1234" } := by
  native_decide

end Lentils.Who.Logic
