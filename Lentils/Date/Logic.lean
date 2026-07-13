/-
Date.Logic — Pure date/time logic for `date`. 0BSD

This file contains ONLY pure functions — no IO, no FFI.
Formal proofs are at the bottom.
No `sorry` or `admit` allowed.

The `date` utility displays the current date and time in various formats.
Per POSIX.1-2017, Section "date — write the date and time":

  The date utility shall write the current date and time to standard output,
  or set the system date and time.

Provenance: POSIX.1-2017, Section "date".
No GPL source was consulted.
-/

namespace Lentils.Date.Logic

/--
A broken-down representation of date and time.
All fields use natural number representation.
-/
structure BrokenDownTime where
  year    : Nat  -- full year (e.g., 2026)
  month   : Nat  -- 1-12
  day     : Nat  -- 1-31
  hour    : Nat  -- 0-23
  minute  : Nat  -- 0-59
  second  : Nat  -- 0-59
  wday    : Nat  -- 0=Sun, 1=Mon, ..., 6=Sat
  yday    : Nat  -- 0-365 (day of year)
  isDST  : Bool
deriving Repr, DecidableEq

/--
Is the given year a leap year?
Per Gregorian calendar rules.
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
Number of days in the given year (365 or 366).
-/
def daysInYear (year : Nat) : Nat :=
  if isLeapYear year then 366 else 365

/--
Find the year corresponding to a given number of days since the Unix epoch (1970-01-01).
Uses bounded iteration to guarantee termination.
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
Compute the number of days from the Unix epoch to the start of the given year.
Uses bounded iteration to guarantee termination.
-/
def daysBeforeYear (year : Nat) : Nat :=
  let rec go (y : Nat) (acc : Nat) (maxIter : Nat) : Nat :=
    if maxIter = 0 then acc
    else if y >= year then acc
    else go (y + 1) (acc + daysInYear y) (maxIter - 1)
  go 1970 0 100000

/--
Compute the day-of-year (0-indexed) from total days since epoch and the year.
-/
def ydayFromTotalDays (totalDays year : Nat) : Nat :=
  totalDays - daysBeforeYear year

/--
Find the month (1-indexed) corresponding to a given day-of-year and year.
Uses bounded iteration to guarantee termination.
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
Compute the day-of-month (1-indexed) from day-of-year, year, and month.
Uses bounded iteration to guarantee termination.
-/
def ydayToDay (yday year month : Nat) : Nat :=
  let rec go (remaining : Nat) (m : Nat) (maxIter : Nat) : Nat :=
    if maxIter = 0 then remaining + 1
    else if m ≥ month then remaining + 1
    else go (remaining - daysInMonth year m) (m + 1) (maxIter - 1)
  go yday 1 12

/--
Convert Unix epoch seconds to a broken-down time structure.
The epoch is 1970-01-01 00:00:00 UTC.
-/
def epochToBrokenDown (epochSecs : Nat) : BrokenDownTime :=
  -- Days since epoch
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

  { year := year, month := month, day := day, hour := hour,
    minute := minute, second := second, wday := wday, yday := yday,
    isDST := false }

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
Month names (abbreviated, 3-letter).
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
Day names (abbreviated, 3-letter).
-/
def dayAbbrev (dow : Nat) : String :=
  match dow with
  | 0 => "Sun" | 1 => "Mon" | 2 => "Tue" | 3 => "Wed"
  | 4 => "Thu" | 5 => "Fri" | 6 => "Sat"
  | _ => "???"

/--
Get the timezone abbreviation for the current local time.
This is a pure approximation; real `date` uses the TZ env var.
For simplicity, we return "UTC".
-/
def timezoneAbbrev : String := "UTC"

/--
Pad a number to at least two digits with leading zeros.
-/
def pad2 (n : Nat) : String :=
  if n < 10 then "0" ++ toString n else toString n

/--
Pad a number to three digits with leading zeros.
-/
def pad3 (n : Nat) : String :=
  if n < 10 then "00" ++ toString n
  else if n < 100 then "0" ++ toString n
  else toString n

/--
Default date/time format: "%a %b %d %H:%M:%S %Z %Y"
Produces output like "Thu Jul 13 21:30:00 UTC 2026".
-/
def defaultFormat (t : BrokenDownTime) : String :=
  let dow := dayAbbrev t.wday
  let mon := monthAbbrev t.month
  s!"{dow} {mon} {pad2 t.day} {pad2 t.hour}:{pad2 t.minute}:{pad2 t.second} {timezoneAbbrev} {t.year}"

/--
Format a broken-down time according to a strftime-style format string.
Supports the following format specifiers:
  %%  literal %
  %Y  full year (4 digits)
  %y  last 2 digits of year
  %m  month (01-12)
  %d  day of month (01-31)
  %H  hour (00-23)
  %I  hour (01-12)
  %M  minute (00-59)
  %S  second (00-59)
  %u  day of week (1=Mon, 7=Sun)
  %w  day of week (0=Sun, 6=Sat)
  %a  abbreviated weekday name
  %A  full weekday name
  %b  abbreviated month name
  %B  full month name
  %j  day of year (001-366)
  %U  week number (Sunday-first, 00-53)
  %W  week number (Monday-first, 00-53)
  %c  locale's date and time (using default format)
  %x  locale's date representation
  %X  locale's time representation
  %Z  timezone abbreviation
  %z  timezone offset (+hhmm)
-/
def formatTime (fmt : String) (t : BrokenDownTime) : String :=
  let rec go (cs : List Char) (acc : String) : String :=
    match cs with
    | [] => acc
    | '%' :: '%' :: rest => go rest (acc ++ "%")
    | '%' :: 'Y' :: rest => go rest (acc ++ toString t.year)
    | '%' :: 'y' :: rest =>
      let y2 := t.year % 100
      go rest (acc ++ pad2 y2)
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
    | '%' :: _ :: rest => go rest acc  -- unknown specifier, skip
    | c :: rest => go rest (acc ++ String.singleton c)
  go fmt.toList ""

/--
Parse a format string. If it starts with '+', the rest is the format;
otherwise, return the default format.
-/
def parseFormat (s : String) : String :=
  if s.startsWith "+" then (s.drop 1).toString else defaultFormat (epochToBrokenDown 0)

/--
Parse seconds since epoch from a string.
Supports "@timestamp" format (Unix timestamp).
-/
def parseTimestamp (s : String) : Option Nat :=
  if s.startsWith "@" then
    match ((s.drop 1).toString).toNat? with
    | some n => some n
    | none => none
  else
    s.toNat?

-- ─── Theorems ──────────────────────────────────────────────────────────────────

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
January 1, 1970 was a Thursday (wday=4).
-/
example : dayOfWeek 1970 1 1 = 4 := by
  native_decide

/--
epochToBrokenDown of 0 gives epoch start.
-/
example : epochToBrokenDown 0 =
  { year := 1970, month := 1, day := 1, hour := 0, minute := 0, second := 0,
    wday := 4, yday := 0, isDST := false } := by
  native_decide

/--
epochToBrokenDown of 86400 gives 1970-01-02 00:00:00 (day 2).
-/
example : epochToBrokenDown 86400 =
  { year := 1970, month := 1, day := 2, hour := 0, minute := 0, second := 0,
    wday := 5, yday := 1, isDST := false } := by
  native_decide

/--
pad2 5 returns "05".
-/
example : pad2 5 = "05" := by
  native_decide

/--
pad2 12 returns "12".
-/
example : pad2 12 = "12" := by
  native_decide

/--
parseTimestamp "@0" returns some 0.
-/
example : parseTimestamp "@0" = some 0 := by
  native_decide

/--
parseTimestamp "@86400" returns some 86400.
-/
example : parseTimestamp "@86400" = some 86400 := by
  native_decide

end Lentils.Date.Logic
