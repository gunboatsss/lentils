/-
Cal.Logic — Pure calendar arithmetic for `cal`. 0BSD

This file contains ONLY pure functions — no IO, no FFI.
Formal proofs are at the bottom.
No `sorry` or `admit` allowed.

The `cal` utility displays a calendar of the specified month or year.
Per POSIX.1-2017, Section "cal — print calendar":

  The cal utility shall write a calendar of the specified month or year
  to standard output. If no operands are specified, the current month is
  written. If a single operand is specified, it is interpreted as a year
  (1-9999); the calendar for that year is written. If two operands are
  specified, the first is the month (1-12) and the second is the year;
  the calendar for that month is written.

Provenance: POSIX.1-2017, Section "cal".
No GPL source was consulted.
-/

namespace Lentils.Cal.Logic

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
  let dayHeader := String.join (dayNames.map (λ n => " " ++ n))
  let rows := weeks.map formatWeek
  header :: dayHeader :: rows
where
  dayNames : List String := ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]

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

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/--
A leap year divisible by 400 is always a leap year.
-/
theorem leap_year_div_400 (y : Nat) (h : y % 400 = 0) : isLeapYear y := by
  unfold isLeapYear
  simp [h]

/--
A year divisible by 100 but not by 400 is not a leap year.
-/
theorem not_leap_year_div_100_not_400 (y : Nat) (h100 : y % 100 = 0) (h400 : y % 400 ≠ 0) : ¬ isLeapYear y := by
  unfold isLeapYear
  simp [h100, h400]

/--
January always has 31 days.
-/
example : daysInMonth 2024 1 = 31 := by
  native_decide

/--
February 2024 (leap year) has 29 days.
-/
example : daysInMonth 2024 2 = 29 := by
  native_decide

/--
February 2023 (non-leap year) has 28 days.
-/
example : daysInMonth 2023 2 = 28 := by
  native_decide

/--
isLeapYear 2024 is true.
-/
example : isLeapYear 2024 := by
  native_decide

/--
isLeapYear 2023 is false.
-/
example : ¬ isLeapYear 2023 := by
  native_decide

/--
isLeapYear 2000 is true (divisible by 400).
-/
example : isLeapYear 2000 := by
  native_decide

/--
isLeapYear 1900 is false (divisible by 100 but not 400).
-/
example : ¬ isLeapYear 1900 := by
  native_decide

/--
January 1, 2024 was a Monday.  dayOfWeek 2024 1 1 = 1.
-/
example : dayOfWeek 2024 1 1 = 1 := by
  native_decide

/--
December 25, 2024 was a Wednesday.  dayOfWeek 2024 12 25 = 3.
-/
example : dayOfWeek 2024 12 25 = 3 := by
  native_decide

/--
January 1, 2000 was a Saturday.  dayOfWeek 2000 1 1 = 6.
-/
example : dayOfWeek 2000 1 1 = 6 := by
  native_decide

/--
parseYear "2024" returns some 2024.
-/
example : parseYear "2024" = some 2024 := by
  native_decide

/--
parseYear "0" returns none (year must be >= 1).
-/
example : parseYear "0" = none := by
  native_decide

/--
parseMonth "12" returns some 12.
-/
example : parseMonth "12" = some 12 := by
  native_decide

/--
parseMonth "13" returns none.
-/
example : parseMonth "13" = none := by
  native_decide

/--
monthName 1 returns "January".
-/
example : monthName 1 = "January" := by
  native_decide

/--
monthName 12 returns "December".
-/
example : monthName 12 = "December" := by
  native_decide

end Lentils.Cal.Logic
