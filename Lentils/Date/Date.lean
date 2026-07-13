/-
Date — IO wrapper for the `date` utility. 0BSD

Displays the current date and time in various formats.
Uses Date.Logic (pure, verified) for formatting.
Uses gettimeofday(2) FFI for current time.
-/

import Lentils.Date.Logic
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Date

open Logic
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/--
Get the current Unix timestamp (seconds since epoch).
Uses the gettimeofday FFI.
-/
def getCurrentTime : IO Nat := do
  let packed ← gettimeofday
  -- Bottom 32 bits are seconds
  let secs := packed &&& 0xFFFFFFFF
  pure secs.toNat

/--
Parse command-line arguments and run `date`.
Supports:
  - no args: display current date/time in default format
  - +format: display with given format string
  - -u: display UTC (default, since our arithmetic is UTC-based)
  - -d @timestamp: display given timestamp
-/
def run (args : List String) : IO UInt32 := do
  let mut fmt : String := ""
  let mut timestamp : Option Nat := none
  let mut rest := args
  let mut hadError : Bool := false

  -- Simple argument parsing loop
  while !rest.isEmpty && !hadError do
    let arg := rest.head?.getD ""
    rest := rest.drop 1
    if arg == "-u" then
      pure ()
    else if arg == "-d" then
      match rest with
      | [] => do
        IO.eprintln "date: option requires an argument -- 'd'"
        hadError := true
        pure ()
      | x :: xs => do
        timestamp := parseTimestamp x
        rest := xs
        pure ()
    else if arg.startsWith "+" then
      fmt := arg
      pure ()
    else
      pure ()

  if hadError then
    return 1

  -- Get the epoch seconds
  let secs ← match timestamp with
    | some s => pure s
    | none => getCurrentTime

  let bdt := epochToBrokenDown secs

  -- Determine format string
  let fmtStr := if fmt.isEmpty then
    defaultFormat bdt
  else
    formatTime (fmt.drop 1).toString bdt

  IO.println fmtStr
  return 0

end Lentils.Date
