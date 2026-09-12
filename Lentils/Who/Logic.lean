/-
Who.Logic — Verified pure logic for `who`.
0BSD

Structure:
  1. State types      — WhoInput (entries + flags)
  2. Specification    — parsing and formatting (single implementation; no duplicate `impl` alias)
  3. Invariants       — parametric properties
  4. Concrete examples

No IO, no FFI, no `sorry` or `admit`.

POSIX: `who` lists logged-in users with optional state and idle info.
-/

import Lentils.Common.Spec

namespace Lentils.Who.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

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
  deriving Repr, DecidableEq, BEq, Inhabited

/--
Input state for who: raw entries and command-line flags.
-/
structure WhoInput where
  rawEntries : List String   -- pipe-delimited entry strings
  flags : List String        -- "-T", "-u", etc.
  deriving Inhabited, BEq

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Helpers
-- ═══════════════════════════════════════════════════════════════════════════════════

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

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

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
Format a single Entry matching GNU who output.
-/
def formatEntry (e : Entry) (timeWidth : Nat) (showState : Bool) (showIdle : Bool) : String :=
  let userPart := padRight e.user 8
  let statePart := if showState then s!" {e.state}" else ""
  let linePart := s!" {padRight e.line 12}"
  let timePart := s!" {padRight e.timeStr timeWidth}"
  let idlePart :=
    if showIdle then
      let idleStr := if e.state = "?" then "?" else formatIdle e.idleSecs
      let minField := if idleStr.length < 3 then 3 else idleStr.length
      let rj := padLeft idleStr minField
      let idlePadded := padRight rj 12
      s!" {idlePadded} {e.pid}"
    else ""
  let hostPart := if e.host.isEmpty then "" else s!" ({e.host})"
  s!"{userPart}{statePart}{linePart}{timePart}{idlePart}{hostPart}"

/--
Format a list of entries as the complete who output.
-/
def formatEntries (entries : List Entry) (showState : Bool := false) (showIdle : Bool := false) : String :=
  let maxTimeWidth := entries.foldl (λ m e => max m e.timeStr.length) 0
  let formatted := entries.map (λ e => formatEntry e maxTimeWidth showState showIdle)
  String.intercalate "\n" formatted

/--
Parse all raw entries, filter valid ones, and format the output.
-/
def format (input : WhoInput) : String :=
  let showState := input.flags.contains "-T"
  let showIdle := input.flags.contains "-u"
  let entries : List Entry := input.rawEntries.filterMap parseEntry
  formatEntries entries showState showIdle

/--
Specification: format who output from raw entries.
-/
def spec (input : WhoInput) : String :=
  format input

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Parametric Invariants
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Parsing a well-formed entry with all fields succeeds.
-/
theorem i_parse_well_formed : parseEntry "root|console|2026-07-14 22:39|myhost|+|3600|1234" =
  some { user := "root", line := "console", timeStr := "2026-07-14 22:39",
         host := "myhost", state := "+", idleSecs := "3600", pid := "1234" } := by
  native_decide

/--
I2: Parsing an entry with wrong number of fields fails.
-/
theorem i_parse_malformed : parseEntry "root|console" = none := by
  native_decide

/--
I3: Parsing empty string fails.
-/
theorem i_parse_empty : parseEntry "" = none := by
  native_decide

/--
I4: padRight pads "hello" to width 10.
-/
theorem i_pad_right_example : padRight "hello" 10 = "hello     " := by
  native_decide

/--
I5: padRight truncates a long string.
-/
theorem i_pad_right_truncates : padRight "hello world" 5 = "hello" := by
  native_decide

/--
I6: formatIdle returns "." for less than 60 seconds.
-/
theorem i_idle_seconds : formatIdle "30" = "." := by
  native_decide

/--
I7: formatIdle returns "?" for invalid input.
-/
theorem i_idle_invalid : formatIdle "abc" = "?" := by
  native_decide

/--
I8: formatIdle returns "00:01" for 60 seconds.
-/
theorem i_idle_one_minute : formatIdle "60" = "00:01" := by
  native_decide

/--
I9: formatIdle returns "01:00" for 3600 seconds.
-/
theorem i_idle_one_hour : formatIdle "3600" = "01:00" := by
  native_decide

/--
I10: formatIdle returns "1day" for 86400 seconds.
-/
theorem i_idle_one_day : formatIdle "86400" = "1day" := by
  native_decide

/--
I11: Empty raw entries produce empty output.
-/
theorem i_empty_entries : format { rawEntries := [], flags := [] } = "" := rfl

/--
Formatting with no flags unfolds to `formatEntries` over the parsed entries
(parametric over all raw entry lists).
-/
theorem format_no_flags_unfold (entries : List String) :
    format { rawEntries := entries, flags := [] } =
    formatEntries (entries.filterMap parseEntry) false false := by
  simp [format]

/--
I13: formatEntry with different timeWidth gives different output.
-/
theorem i_timewidth_matters : formatEntry
    { user := "root", line := "console", timeStr := "2026-07-14 22:39",
      host := "myhost", state := "+", idleSecs := "3600", pid := "1234" } 10 false false ≠
  formatEntry
    { user := "root", line := "console", timeStr := "2026-07-14 22:39",
      host := "myhost", state := "+", idleSecs := "3600", pid := "1234" } 20 false false := by
  native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 7. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- Parsing a well-formed entry. -/
example : parseEntry "root|console|2026-07-14 22:39|myhost|+|3600|1234" =
  some { user := "root", line := "console", timeStr := "2026-07-14 22:39",
         host := "myhost", state := "+", idleSecs := "3600", pid := "1234" } :=
  i_parse_well_formed

/-- Short idle time. -/
example : formatIdle "30" = "." := i_idle_seconds

/-- Invalid idle. -/
example : formatIdle "abc" = "?" := i_idle_invalid

end Lentils.Who.Logic
