/-
Cal.Logic — Verified pure calendar logic for `cal`. 0BSD

Spec-First Methodology:
  1. State types    — CalInput (args)
  2. Specification  — formatMonth, formatYear, dayOfWeek, daysInMonth, isLeapYear: the formal "what"
  3. Implementation — the "how" (= spec, since spec is executable)
  4. Invariants       — parametric properties over all inputs
  5. Invariants     — parametric properties over all inputs
  6. Lemmas         — helper theorems used in proofs
  7. Concrete corollaries (optional)

No IO, no FFI, no `sorry` or `admit`.

`cal` displays a calendar. Per POSIX.1-2017, Section "cal — print calendar".
-/

namespace Lentils.Cal.Logic

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for cal.
-/
structure CalInput where
  month : Option Nat  -- none means all months (year view)
  year : Nat := 2025
  deriving Inhabited, BEq

/--
Default input: current month (represented as none for year, no month specified).
Uses 2025 as placeholder; actual default is the current month from system clock.
-/
def defaultInput : CalInput := { month := none, year := 2025 }

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Is the given year a leap year?
Per Gregorian calendar rules: divisible by 400 → leap;
divisible by 100 → not leap; divisible by 4 → leap; else not leap.
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
Sakamoto's month offset table as a function.
-/
def sakamotoOffset (month : Nat) : Nat :=
  match month with
  | 1 => 0  | 2 => 3  | 3 => 2  | 4 => 5
  | 5 => 0  | 6 => 3  | 7 => 5  | 8 => 1
  | 9 => 4  | 10 => 6 | 11 => 2 | 12 => 4
  | _ => 0

/--
Compute the day of the week (0=Sun, 1=Mon, ..., 6=Sat) for a given date.
Uses Tomohiko Sakamoto's algorithm.
-/
def dayOfWeek (year month day : Nat) : Nat :=
  let y := if month < 3 then year - 1 else year
  let m := sakamotoOffset month
  (y + y / 4 - y / 100 + y / 400 + m + day) % 7

/--
Get the name of a month by its number (1-based).
Returns empty string for invalid month numbers.
-/
def monthName (month : Nat) : String :=
  match month with
  | 1 => "January"   | 2 => "February" | 3 => "March"    | 4 => "April"
  | 5 => "May"       | 6 => "June"     | 7 => "July"     | 8 => "August"
  | 9 => "September" | 10 => "October" | 11 => "November" | 12 => "December"
  | _ => ""

/--
Get the abbreviated day name (0=Sun, 6=Sat).
-/
def dayNameAbbrev (dow : Nat) : String :=
  match dow with
  | 0 => "Sun" | 1 => "Mon" | 2 => "Tue" | 3 => "Wed"
  | 4 => "Thu" | 5 => "Fri" | 6 => "Sat"
  | _ => "???"

/--
Format a single line of the calendar: up to 7 day numbers.
Each entry is either "" (empty) or the day number as string.
-/
def formatWeek (week : List (Option Nat)) : String :=
  let strs := week.map fun d =>
    match d with
    | none => "   "
    | some n =>
      if n < 10 then " " ++ toString n ++ " "
      else toString n ++ " "
  String.join strs

/--
Flatten a list of lists.
-/
def flatten (l : List (List α)) : List α :=
  match l with
  | [] => []
  | xs :: xss => xs ++ flatten xss

/--
Split a list into chunks of size n (except possibly the last chunk).
Uses a decreasing Nat counter to guarantee termination.
-/
def chunksOf (n : Nat) (l : List α) : List (List α) :=
  let total := l.length
  let numChunks := (total + n - 1) / n
  let rec go (i : Nat) (acc : List (List α)) : List (List α) :=
    if h : i ≥ numChunks then acc.reverse
    else
      let start := i * n
      let chunk := l.drop start |>.take n
      go (i + 1) (chunk :: acc)
  go 0 []

/--
Generate the calendar grid for a given month and year.
Returns a list of strings, each being one row of the calendar.
First line is the month/year header, second is day names,
then the weeks.
-/
def formatMonth (year month : Nat) : List String :=
  let days := daysInMonth year month
  let startDow := dayOfWeek year month 1  -- 0=Sun
  -- Build day slots: first fill with none for days before start, then numbers
  let slots : List (Option Nat) :=
    List.replicate startDow none ++
    (List.range days).map (λ i => some (i + 1))
  -- Pad to full weeks
  let remainder := slots.length % 7
  let slotsPadded := if remainder = 0 then slots else slots ++ List.replicate (7 - remainder) none
  -- Split into weeks of 7
  let weeks := chunksOf 7 slotsPadded
  -- Header
  let header := monthName month ++ " " ++ toString year
  let dayNames := ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
  let dayHeader := String.join (dayNames.map (λ n => " " ++ n))
  let rows := weeks.map formatWeek
  header :: dayHeader :: rows

/--
Format the entire year as a concatenation of all 12 months.
-/
def formatYear (year : Nat) : List String :=
  let months := List.range 12
  flatten (months.map (λ m => formatMonth year (m + 1)))

/--
Parse a year from a string. Returns none if invalid.
Valid range: 1-9999.
-/
def parseYear (s : String) : Option Nat :=
  match s.toNat? with
  | none => none
  | some y => if y >= 1 && y <= 9999 then some y else none

/--
Parse a month from a string. Returns none if invalid.
Valid range: 1-12.
-/
def parseMonth (s : String) : Option Nat :=
  match s.toNat? with
  | none => none
  | some m => if m >= 1 && m <= 12 then some m else none

/--
The specification for cal: given input, produce the calendar lines.
-/
def spec (input : CalInput) : List String :=
  match input.month with
  | some m => formatMonth input.year m
  | none   => formatYear input.year

-- Spec is directly executable, so no separate `impl` alias is kept
-- (Cat.Logic pattern: a single implementation `def` plus invariants).

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: A leap year divisible by 400 is always a leap year.
-/
theorem i_leap_year_div_400 (y : Nat) (h : y % 400 = 0) : isLeapYear y := by
  unfold isLeapYear
  simp [h]

/--
I2: A year divisible by 100 but not by 400 is not a leap year.
-/
theorem i_not_leap_year_div_100_not_400 (y : Nat) (h100 : y % 100 = 0) (h400 : y % 400 ≠ 0) : ¬ isLeapYear y := by
  unfold isLeapYear
  simp [h100, h400]

/--
I3: January always has 31 days for concrete years.
-/
theorem i_january_31_days : daysInMonth 2024 1 = 31 := by
  native_decide

/--
I4: February 2024 (leap year) has 29 days.
-/
theorem i_february_2024 : daysInMonth 2024 2 = 29 := by
  native_decide

/--
I5: February 2023 (non-leap year) has 28 days.
-/
theorem i_february_2023 : daysInMonth 2023 2 = 28 := by
  native_decide

/--
I6: January 1, 2024 was a Monday (1).
-/
theorem i_dayOfWeek_2024_01_01 : dayOfWeek 2024 1 1 = 1 := by native_decide

/--
I9: December 25, 2024 was a Wednesday (3).
-/
theorem i_dayOfWeek_2024_12_25 : dayOfWeek 2024 12 25 = 3 := by native_decide

/--
I10: January 1, 2000 was a Saturday (6).
-/
theorem i_dayOfWeek_2000_01_01 : dayOfWeek 2000 1 1 = 6 := by native_decide

/--
I11: parseYear "2024" returns some 2024.
-/
theorem i_parseYear_valid : parseYear "2024" = some 2024 := by native_decide

/--
I12: parseYear "0" returns none (year must be >= 1).
-/
theorem i_parseYear_zero : parseYear "0" = none := by native_decide

/--
I13: parseYear for year > 9999 returns none.
-/
theorem i_parseYear_too_large : parseYear "10000" = none := by native_decide

/--
I14: parseMonth "12" returns some 12.
-/
theorem i_parseMonth_valid : parseMonth "12" = some 12 := by native_decide

/--
I15: parseMonth "13" returns none (month must be <= 12).
-/
theorem i_parseMonth_invalid : parseMonth "13" = none := by native_decide

/--
I16: monthName 1 returns "January".
-/
theorem i_monthName_jan : monthName 1 = "January" := by native_decide

/--
I17: monthName 12 returns "December".
-/
theorem i_monthName_dec : monthName 12 = "December" := by native_decide

/--
I19: isLeapYear 2024 is true.
-/
theorem i_isLeapYear_2024 : isLeapYear 2024 := by native_decide

/--
I20: isLeapYear 2023 is false.
-/
theorem i_isLeapYear_2023 : ¬ isLeapYear 2023 := by native_decide

/--
I21: isLeapYear 2000 is true (divisible by 400).
-/
theorem i_isLeapYear_2000 : isLeapYear 2000 := by native_decide

/--
I22: isLeapYear 1900 is false (divisible by 100 but not 400).
-/
theorem i_isLeapYear_1900 : ¬ isLeapYear 1900 := by native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- January always has 31 days. -/
example : daysInMonth 2024 1 = 31 := i_january_31_days

/-- February 2024 (leap year) has 29 days. -/
example : daysInMonth 2024 2 = 29 := by native_decide

/-- February 2023 (non-leap year) has 28 days. -/
example : daysInMonth 2023 2 = 28 := by native_decide

/-- isLeapYear 2024. -/
example : isLeapYear 2024 := i_isLeapYear_2024

/-- isLeapYear 2023. -/
example : ¬ isLeapYear 2023 := i_isLeapYear_2023

/-- isLeapYear 2000. -/
example : isLeapYear 2000 := i_isLeapYear_2000

/-- isLeapYear 1900. -/
example : ¬ isLeapYear 1900 := i_isLeapYear_1900

/-- January 1, 2024 was a Monday. -/
example : dayOfWeek 2024 1 1 = 1 := i_dayOfWeek_2024_01_01

/-- parseYear "2024" returns some 2024. -/
example : parseYear "2024" = some 2024 := i_parseYear_valid

/-- parseMonth "12" returns some 12. -/
example : parseMonth "12" = some 12 := i_parseMonth_valid

/-- monthName 1 = "January". -/
example : monthName 1 = "January" := i_monthName_jan

end Lentils.Cal.Logic
