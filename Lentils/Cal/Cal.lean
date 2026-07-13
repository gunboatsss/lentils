/-
Cal — IO wrapper for the `cal` utility. 0BSD

Displays a calendar of the specified month or year.
Uses Cal.Logic (pure, verified) for all date arithmetic.
Uses gettimeofday(2) FFI to determine the current year/month when
no arguments are given.
-/

import Lentils.Cal.Logic
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Cal

open Logic
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/--
Get the current year and month from the system clock via gettimeofday.
Returns (year, month) as Nats.
-/
def getCurrentYearMonth : IO (Nat × Nat) := do
  let packed ← gettimeofday
  let secs := (packed &&& 0xFFFFFFFF).toNat
  -- Derived from epochToBrokenDown (but we keep this self-contained to avoid
  -- depending on the Date module)
  let totalDays := secs / 86400
  -- Find year with bounded recursion (max 100000 iterations)
  let rec findYear (d : Nat) (y : Nat) (maxIter : Nat) : Nat :=
    if maxIter = 0 then y
    else
      let dim := if Lentils.Cal.Logic.isLeapYear y then 366 else 365
      if d < dim then y
      else findYear (d - dim) (y + 1) (maxIter - 1)
  let year := findYear totalDays 1970 100000
  -- Days into the year (0-indexed)
  let rec daysInYearsBefore (y : Nat) (acc : Nat) : Nat :=
    if y <= 1970 then acc
    else
      let prev := y - 1
      daysInYearsBefore prev (acc + (if Lentils.Cal.Logic.isLeapYear prev then 366 else 365))
  let startOfYear := daysInYearsBefore year 0
  let yday := totalDays - startOfYear
  -- Find month with bounded recursion
  let rec findMonth (d : Nat) (m : Nat) (maxIter : Nat) : Nat :=
    if maxIter = 0 then m
    else
      let dim := Lentils.Cal.Logic.daysInMonth year m
      if d < dim ∨ m ≥ 12 then m
      else findMonth (d - dim) (m + 1) (maxIter - 1)
  let month := findMonth yday 1 12
  pure (year, month)

/--
Run the `cal` utility with the given arguments.
-/
def run (args : List String) : IO UInt32 := do
  let nonFlagArgs := args.filter (λ a => !a.startsWith "-")
  match nonFlagArgs with
  | [] => do
    -- No args: show current month
    let (year, month) ← getCurrentYearMonth
    let lines := formatMonth year month
    for line in lines do
      IO.println line
    return 0
  | [y] => do
    -- Single arg: show all months of the given year
    match parseYear y with
    | none =>
      exitError "cal" (some y) "invalid year (use 1-9999)"
    | some year =>
      let lines := formatYear year
      for line in lines do
        IO.println line
      return 0
  | [m, y] => do
    -- Two args: show specific month/year
    match parseMonth m, parseYear y with
    | none, _ =>
      exitError "cal" (some m) "invalid month (use 1-12)"
    | _, none =>
      exitError "cal" (some y) "invalid year (use 1-9999)"
    | some month, some year =>
      let lines := formatMonth year month
      for line in lines do
        IO.println line
      return 0
  | _ =>
    exitError "cal" none "too many arguments"

end Lentils.Cal
