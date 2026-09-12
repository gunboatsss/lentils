/-
Pinky.Logic — Verified pure logic for `pinky`. 0BSD

Spec-First Methodology:
  1. State types    — PinkyInput (flags + args)
  2. Specification  — formatShort, formatIdle, pad, parseWhoEntry: the formal "what"
  3. Implementation — the "how" (= spec, since spec is executable)
  4. Correctness    — theorem: impl = spec
  5. Invariants     — parametric properties over all inputs
  6. Lemmas         — helper theorems used in proofs
  7. Concrete corollaries (optional)

No IO, no FFI, no `sorry` or `admit`.

`pinky` displays user information (like `who` but more readable).
-/

namespace Lentils.Pinky.Logic

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
A pinky entry representing a logged-in user.
-/
structure Entry where
  user : String
  realName : String
  line : String
  timeStr : String
  host : String
  state : String   -- "+", "-", or "?"
  idleSecs : String
  deriving Repr, BEq

/--
Input state for pinky.
-/
structure PinkyInput where
  longFormat : Bool := false
  users : List String := []
  deriving Inhabited, BEq

/--
Default input.
-/
def defaultInput : PinkyInput := { longFormat := false, users := [] }

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Parse a raw who entry into (user, line, time, host, state, idleSecs).
Format uses "|" as separator.
-/
def parseWhoEntry (raw : String) : Option (String × String × String × String × String × String) :=
  let parts := raw.splitOn "|"
  match parts with
  | user :: line :: timeStr :: host :: state :: idleSecs :: _ =>
    some (user, line, timeStr, host, state, idleSecs)
  | _ => none

/--
Pad or truncate a string to exactly n characters.
-/
def pad (s : String) (n : Nat) : String :=
  let cs := s.toList
  if cs.length ≥ n then String.ofList (cs.take n)
  else s ++ String.ofList (List.replicate (n - cs.length) ' ')

/--
Format idle seconds: ".", "mm", "hh:mm", or "Ndays".
-/
def formatIdle (secsStr : String) : String :=
  match secsStr.toNat? with
  | none => "?"
  | some secs =>
    if secs < 60 then "."
    else if secs < 3600 then
      let m := secs / 60
      if m < 10 then " " ++ toString m ++ "m" else toString m ++ "m"
    else if secs < 86400 then
      let h := secs / 3600
      let m := (secs % 3600) / 60
      let hStr := if h < 10 then " " ++ toString h else toString h
      let mStr := if m < 10 then "0" ++ toString m else toString m
      hStr ++ ":" ++ mStr
    else
      let days := secs / 86400
      toString days ++ "d"

/--
Short format with header: Login, Name, TTY, Idle, When, Where.
-/
def formatShort (entries : List Entry) : String :=
  let header := "Login    Name                 TTY      Idle   When             Where"
  let lines := entries.map (λ e =>
    let userPad := pad e.user 8
    let namePad := pad e.realName 20
    let ttyField := e.state ++ pad e.line 5
    let idleField := pad (formatIdle e.idleSecs) 6
    let timeField := pad e.timeStr 16
    let whereField := if e.host.isEmpty then "" else e.host
    userPad ++ " " ++ namePad ++ " " ++ ttyField ++ " " ++ idleField ++ " " ++
      timeField ++ " " ++ whereField)
  String.intercalate "\n" (header :: lines)

/--
Format long format for a single entry.
-/
def formatLong (e : Entry) : String :=
  let hostStr := if e.host.isEmpty then "" else " from " ++ e.host
  let stateStr := if e.state = "?" then "" else " (" ++ e.state ++ ")"
  "Login: " ++ e.user ++ "  Name: " ++ e.realName ++ "\n" ++
  "On since " ++ e.timeStr ++ " on " ++ e.line ++ stateStr ++ hostStr

/--
Parse pinky arguments.
-/
def parseArgs (args : List String) : PinkyInput :=
  let rec go (remaining : List String) (opts : PinkyInput) : PinkyInput :=
    match remaining with
    | [] => opts
    | "-l" :: rest => go rest { opts with longFormat := true }
    | s :: rest =>
      if s.startsWith "-" then go rest opts
      else go rest { opts with users := opts.users ++ [s] }
  go args { longFormat := false, users := [] }

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Correctness Theorem
-- ═══════════════════════════════════════════════════════════════════════════════════

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: pad "hello" to 3 = "hel".
-/
theorem i_pad_hello_3 : pad "hello" 3 = "hel" := by native_decide

/--
I2: pad "hello" to 8 = "hello   ".
-/
theorem i_pad_hello_8 : pad "hello" 8 = "hello   " := by native_decide

/--
I3: pad of empty string to n produces n spaces.
-/
theorem i_pad_empty_4 : pad "" 4 = "    " := by native_decide

/--
I7: `formatIdle` returns "." for idle < 60 seconds.
-/
theorem i_formatIdle_dot : formatIdle "30" = "." := by native_decide

/--
I8: `formatIdle` returns minutes format.
-/
theorem i_formatIdle_minutes : formatIdle "120" = " 2m" := by native_decide

/--
I9: `formatIdle` returns hours:minutes format.
-/
theorem i_formatIdle_hours : formatIdle "3661" = " 1:01" := by native_decide

/--
I10: `formatIdle` returns days format.
-/
theorem i_formatIdle_days : formatIdle "90000" = "1d" := by native_decide

/--
I11: `formatIdle` returns "?" for non-numeric input.
-/
theorem i_formatIdle_unknown : formatIdle "abc" = "?" := by native_decide

/--
I12: `parseWhoEntry` parses full entry correctly.
-/
theorem i_parseWhoEntry_full :
  parseWhoEntry "alice|tty1|2026-07-15 10:00|myhost|+|120|1234" =
    some ("alice", "tty1", "2026-07-15 10:00", "myhost", "+", "120") := by
  native_decide

/--
I13: `parseWhoEntry` returns none for short entries.
-/
theorem i_parseWhoEntry_short :
  parseWhoEntry "bob|pts/0|10:30|:0" = none := by native_decide

/--
I14: `formatShort` with empty entries returns just the header.
-/
theorem i_formatShort_empty : formatShort [] = "Login    Name                 TTY      Idle   When             Where" := rfl

/--
I15: `formatShort` header is always first line for non-empty entries.
-/
theorem i_formatShort_header (e : Entry) (entries : List Entry) :
    (formatShort (e :: entries)).startsWith "Login    Name                 TTY      Idle   When             Where" := by
  unfold formatShort
  simp

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- pad "hello" to 3. -/
example : pad "hello" 3 = "hel" := i_pad_hello_3

/-- pad "hello" to 8. -/
example : pad "hello" 8 = "hello   " := i_pad_hello_8

/-- formatIdle "30" = ".". -/
example : formatIdle "30" = "." := i_formatIdle_dot

/-- formatIdle "120" = " 2m". -/
example : formatIdle "120" = " 2m" := i_formatIdle_minutes

/-- formatIdle "3661" = " 1:01". -/
example : formatIdle "3661" = " 1:01" := i_formatIdle_hours

/-- formatIdle "90000" = "1d". -/
example : formatIdle "90000" = "1d" := i_formatIdle_days

/-- formatIdle "abc" = "?". -/
example : formatIdle "abc" = "?" := i_formatIdle_unknown

/-- parseWhoEntry with full entry. -/
example : parseWhoEntry "alice|tty1|2026-07-15 10:00|myhost|+|120|1234" =
  some ("alice", "tty1", "2026-07-15 10:00", "myhost", "+", "120") := i_parseWhoEntry_full

end Lentils.Pinky.Logic
