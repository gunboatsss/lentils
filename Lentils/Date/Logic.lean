/-
Date.Logic — Verified pure date/time logic for `date`. 0BSD

Spec-First Methodology:
  1. State types    — DateInput (format + timestamp), BrokenDownTime
  2. Specification  — formatTime, defaultFormat, epochToBrokenDown: the formal "what"
  3. Implementation — the "how" (= spec, since spec is executable)
  4. Invariants       — parametric properties over all inputs
  5. Invariants     — parametric properties over all inputs
  6. Lemmas         — helper theorems used in proofs
  7. Concrete corollaries (optional)

No IO, no FFI, no `sorry` or `admit`.
-/

namespace Lentils.Date.Logic

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
A broken-down representation of date and time.
-/
structure BrokenDownTime where
  year    : Nat
  month   : Nat
  day     : Nat
  hour    : Nat
  minute  : Nat
  second  : Nat
  wday    : Nat
  yday    : Nat
  isDST  : Bool
deriving Repr, DecidableEq, BEq

/--
Input state for date.
-/
structure DateInput where
  format : String := ""
  timestamp : Option Nat := none
  utc : Bool := true
  deriving Inhabited, BEq

/--
Default input.
-/
def defaultInput : DateInput := {}

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Is the given year a leap year?
-/
def isLeapYear (year : Nat) : Bool :=
  (year % 400 = 0) || (year % 100 != 0 && year % 4 = 0)

/--
Number of days in a given month (1-12) for the given year.
-/
def daysInMonth (year month : Nat) : Nat :=
  match month with
  | 1 => 31
  | 2 => if isLeapYear year then 29 else 28
  | 3 => 31
  | 4 => 30
  | 5 => 31
  | 6 => 30
  | 7 => 31
  | 8 => 31
  | 9 => 30
  | 10 => 31
  | 11 => 30
  | 12 => 31
  | _ => 0

/--
Sakamoto's month offset table.
-/
def sakamotoOffset (month : Nat) : Nat :=
  match month with
  | 1 => 0  | 2 => 3  | 3 => 2  | 4 => 5
  | 5 => 0  | 6 => 3  | 7 => 5  | 8 => 1
  | 9 => 4  | 10 => 6 | 11 => 2 | 12 => 4
  | _ => 0

/--
Compute the day of the week (0=Sun) for a given date.
-/
def dayOfWeek (year month day : Nat) : Nat :=
  let y := if month < 3 then year - 1 else year
  let m := sakamotoOffset month
  (y + y / 4 - y / 100 + y / 400 + m + day) % 7

/--
Number of days in the given year.
-/
def daysInYear (year : Nat) : Nat :=
  if isLeapYear year then 366 else 365

/--
Find the year from days since Unix epoch.
-/
def epochDaysToYear (totalDays : Nat) : Nat :=
  let rec go (remaining : Nat) (currentYear : Nat) (maxIter : Nat) : Nat :=
    if maxIter = 0 then currentYear
    else
      let dim := daysInYear currentYear
      if remaining < dim then currentYear
      else go (remaining - dim) (currentYear + 1) (maxIter - 1)
  go totalDays 1970 100000

/--
Days from epoch to start of given year.
-/
def daysBeforeYear (year : Nat) : Nat :=
  let rec go (y : Nat) (acc : Nat) (maxIter : Nat) : Nat :=
    if maxIter = 0 then acc
    else if y >= year then acc
    else go (y + 1) (acc + daysInYear y) (maxIter - 1)
  go 1970 0 100000

/--
Day-of-year from total days and year.
-/
def ydayFromTotalDays (totalDays year : Nat) : Nat :=
  totalDays - daysBeforeYear year

/--
Find month from day-of-year.
-/
def ydayToMonth (yday year : Nat) : Nat :=
  let rec go (remaining : Nat) (month : Nat) (maxIter : Nat) : Nat :=
    if maxIter = 0 then month
    else
      let dim := daysInMonth year month
      if remaining < dim ∨ month ≥ 12 then month
      else go (remaining - dim) (month + 1) (maxIter - 1)
  go yday 1 12

/--
Find day-of-month from day-of-year.
-/
def ydayToDay (yday year month : Nat) : Nat :=
  let rec go (remaining : Nat) (m : Nat) (maxIter : Nat) : Nat :=
    if maxIter = 0 then remaining + 1
    else if m ≥ month then remaining + 1
    else go (remaining - daysInMonth year m) (m + 1) (maxIter - 1)
  go yday 1 12

/--
Convert Unix epoch seconds to broken-down time.
-/
def epochToBrokenDown (epochSecs : Nat) : BrokenDownTime :=
  let totalDays := epochSecs / 86400
  let remainingSecs := epochSecs % 86400
  let hour := remainingSecs / 3600
  let minute := (remainingSecs % 3600) / 60
  let second := remainingSecs % 60
  let year := epochDaysToYear totalDays
  let yday := ydayFromTotalDays totalDays year
  let month := ydayToMonth yday year
  let day := ydayToDay yday year month
  let wday := dayOfWeek year month day
  { year, month, day, hour, minute, second, wday, yday, isDST := false }

/--
Month names (full).
-/
def monthName (month : Nat) : String :=
  match month with
  | 1 => "January"   | 2 => "February" | 3 => "March"    | 4 => "April"
  | 5 => "May"       | 6 => "June"     | 7 => "July"     | 8 => "August"
  | 9 => "September" | 10 => "October" | 11 => "November" | 12 => "December"
  | _ => ""

/--
Month names (abbreviated).
-/
def monthAbbrev (month : Nat) : String :=
  match month with
  | 1 => "Jan" | 2 => "Feb" | 3 => "Mar" | 4 => "Apr"
  | 5 => "May" | 6 => "Jun" | 7 => "Jul" | 8 => "Aug"
  | 9 => "Sep" | 10 => "Oct" | 11 => "Nov" | 12 => "Dec"
  | _ => ""

/--
Day names (full).
-/
def dayName (dow : Nat) : String :=
  match dow with
  | 0 => "Sunday"   | 1 => "Monday" | 2 => "Tuesday" | 3 => "Wednesday"
  | 4 => "Thursday" | 5 => "Friday" | 6 => "Saturday"
  | _ => "???"

/--
Day names (abbreviated).
-/
def dayAbbrev (dow : Nat) : String :=
  match dow with
  | 0 => "Sun" | 1 => "Mon" | 2 => "Tue" | 3 => "Wed"
  | 4 => "Thu" | 5 => "Fri" | 6 => "Sat"
  | _ => "???"

/--
Timezone abbreviation (UTC).
-/
def timezoneAbbrev : String := "UTC"

/--
Pad a number to at least two digits.
-/
def pad2 (n : Nat) : String :=
  if n < 10 then "0" ++ toString n else toString n

/--
Pad a number to three digits.
-/
def pad3 (n : Nat) : String :=
  if n < 10 then "00" ++ toString n
  else if n < 100 then "0" ++ toString n
  else toString n

/--
Default format.
-/
def defaultFormat (t : BrokenDownTime) : String :=
  s!"{dayAbbrev t.wday} {monthAbbrev t.month} {pad2 t.day} {pad2 t.hour}:{pad2 t.minute}:{pad2 t.second} {timezoneAbbrev} {t.year}"

/--
Format time according to format string.
-/
def formatTime (fmt : String) (t : BrokenDownTime) : String :=
  let rec go (cs : List Char) (acc : String) : String :=
    match cs with
    | [] => acc
    | '%' :: '%' :: rest => go rest (acc ++ "%")
    | '%' :: 'Y' :: rest => go rest (acc ++ toString t.year)
    | '%' :: 'y' :: rest => go rest (acc ++ pad2 (t.year % 100))
    | '%' :: 'm' :: rest => go rest (acc ++ pad2 t.month)
    | '%' :: 'd' :: rest => go rest (acc ++ pad2 t.day)
    | '%' :: 'H' :: rest => go rest (acc ++ pad2 t.hour)
    | '%' :: 'I' :: rest =>
      let h12 := if t.hour % 12 = 0 then 12 else t.hour % 12
      go rest (acc ++ pad2 h12)
    | '%' :: 'M' :: rest => go rest (acc ++ pad2 t.minute)
    | '%' :: 'S' :: rest => go rest (acc ++ pad2 t.second)
    | '%' :: 'u' :: rest =>
      let u := if t.wday = 0 then 7 else t.wday
      go rest (acc ++ toString u)
    | '%' :: 'w' :: rest => go rest (acc ++ toString t.wday)
    | '%' :: 'a' :: rest => go rest (acc ++ dayAbbrev t.wday)
    | '%' :: 'A' :: rest => go rest (acc ++ dayName t.wday)
    | '%' :: 'b' :: rest => go rest (acc ++ monthAbbrev t.month)
    | '%' :: 'B' :: rest => go rest (acc ++ monthName t.month)
    | '%' :: 'j' :: rest => go rest (acc ++ pad3 (t.yday + 1))
    | '%' :: 'U' :: rest =>
      let weekNum := (t.yday + 7 - t.wday) / 7
      go rest (acc ++ pad2 weekNum)
    | '%' :: 'W' :: rest =>
      let monWday := if t.wday = 0 then 6 else t.wday - 1
      let weekNum := (t.yday + 7 - monWday) / 7
      go rest (acc ++ pad2 weekNum)
    | '%' :: 'c' :: rest => go rest (acc ++ defaultFormat t)
    | '%' :: 'x' :: rest =>
      go rest (acc ++ pad2 t.month ++ "/" ++ pad2 t.day ++ "/" ++ pad2 (t.year % 100))
    | '%' :: 'X' :: rest =>
      go rest (acc ++ pad2 t.hour ++ ":" ++ pad2 t.minute ++ ":" ++ pad2 t.second)
    | '%' :: 'Z' :: rest => go rest (acc ++ timezoneAbbrev)
    | '%' :: 'z' :: rest => go rest (acc ++ "+0000")
    | '%' :: _ :: rest => go rest acc
    | c :: rest => go rest (acc ++ String.singleton c)
  go fmt.toList ""

/--
Parse timestamp from "@..." format.
-/
def parseTimestamp (s : String) : Option Nat :=
  if s.startsWith "@" then
    match ((s.drop 1).toString).toNat? with
    | some n => some n
    | none => none
  else
    s.toNat?

/--
Specification.
-/
def spec (input : DateInput) (getCurrentTime : Nat) : String :=
  let secs := match input.timestamp with
    | some s => s
    | none => getCurrentTime
  let bdt := epochToBrokenDown secs
  if input.format.isEmpty then
    defaultFormat bdt
  else
    formatTime input.format bdt

-- Spec is directly executable, so no separate `impl` alias is kept
-- (Cat.Logic pattern: a single implementation `def` plus invariants).

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: isLeapYear 2024 is true.
-/
theorem i_isLeapYear_2024 : isLeapYear 2024 := by native_decide

/--
I2: isLeapYear 2023 is false.
-/
theorem i_isLeapYear_2023 : ¬ isLeapYear 2023 := by native_decide

/--
I3: epochToBrokenDown of 0 gives epoch start.
-/
theorem i_epochToBrokenDown_zero : epochToBrokenDown 0 =
  { year := 1970, month := 1, day := 1, hour := 0, minute := 0, second := 0,
    wday := 4, yday := 0, isDST := false } := by
  native_decide

/--
I4: epochToBrokenDown of 86400 gives next day.
-/
theorem i_epochToBrokenDown_86400 : epochToBrokenDown 86400 =
  { year := 1970, month := 1, day := 2, hour := 0, minute := 0, second := 0,
    wday := 5, yday := 1, isDST := false } := by
  native_decide

/--
I5: pad2 5 returns "05".
-/
theorem i_pad2_5 : pad2 5 = "05" := by native_decide

/--
I6: pad2 12 returns "12".
-/
theorem i_pad2_12 : pad2 12 = "12" := by native_decide

/--
I7: parseTimestamp "@0" returns some 0.
-/
theorem i_parseTimestamp_0 : parseTimestamp "@0" = some 0 := by native_decide

/--
I8: parseTimestamp "@86400" returns some 86400.
-/
theorem i_parseTimestamp_86400 : parseTimestamp "@86400" = some 86400 := by native_decide

/--
I9: formatTime with %% produces a literal %.
-/
theorem i_formatTime_percent (t : BrokenDownTime) : formatTime "%%" t = "%" := rfl

/--
I10: formatTime with %Y produces the full year.
-/
theorem i_formatTime_Y (t : BrokenDownTime) : formatTime "%Y" t = toString t.year := rfl

/--
I11: formatTime with empty format string produces empty string.
-/
theorem i_formatTime_empty (t : BrokenDownTime) : formatTime "" t = "" := rfl

/--
I12: defaultFormat for epoch start is known.
-/
theorem i_defaultFormat_epoch : defaultFormat (epochToBrokenDown 0) = "Thu Jan 01 00:00:00 UTC 1970" := by
  native_decide

/--
I13: Spec with empty format uses the default format (∀-quantified invariant).
-/
theorem i_spec_empty_format (ts now : Nat) :
    spec { format := "", timestamp := some ts, utc := true } now =
      defaultFormat (epochToBrokenDown ts) := by
  simp [spec]

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- isLeapYear 2024. -/
example : isLeapYear 2024 := i_isLeapYear_2024

/-- epochToBrokenDown of 0. -/
example : epochToBrokenDown 0 = {
  year := 1970, month := 1, day := 1, hour := 0, minute := 0, second := 0,
  wday := 4, yday := 0, isDST := false } := i_epochToBrokenDown_zero

/-- pad2 5 = "05". -/
example : pad2 5 = "05" := i_pad2_5

/-- parseTimestamp "@0" = 0. -/
example : parseTimestamp "@0" = some 0 := i_parseTimestamp_0

end Lentils.Date.Logic
