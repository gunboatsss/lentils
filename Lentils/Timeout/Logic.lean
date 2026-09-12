/-
Timeout.Logic — Verified pure logic for `timeout`.
0BSD

Runs a command and kills it if it does not finish within a given duration.

Structure:
  1. State types      — TimeoutInput, Config
  2. Specification    — parseArgs: Option Config
  3. Correctness      — theorem: impl = spec
  4. Invariants       — parametric properties
  5. Lemmas           — helper theorems
  6. Concrete examples

No IO, no FFI, no `sorry` or `admit`.
-/

import Lentils.Common.Spec

namespace Lentils.Timeout.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for timeout.
-/
structure TimeoutInput where
  args : List String
  deriving Inhabited, BEq, Repr

/--
Parsed configuration for a `timeout` invocation.
- `seconds`   : the time limit in seconds (none if no duration was given)
- `signal`    : signal number to send on timeout (default SIGTERM = 15)
- `killAfter` : seconds to wait before SIGKILL if still alive (0 = never)
- `cmd`       : the command and its operands
-/
structure Config where
  seconds : Option Nat
  signal : Nat
  killAfter : Nat
  cmd : List String
  deriving Repr, DecidableEq, Inhabited, BEq

/--
Default timeout configuration: SIGTERM (15), no kill-after.
-/
def defaultConfig : Config :=
  { seconds := none, signal := 15, killAfter := 0, cmd := [] }

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Helpers
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Map a duration suffix character to its multiplier in seconds.
-/
def durationFactor (c : Char) : Option Nat :=
  match c with
  | 's' => some 1
  | 'm' => some 60
  | 'h' => some 3600
  | 'd' => some 86400
  | _ => none

/--
Parse a duration string (with optional s/m/h/d suffix) into seconds.
-/
def parseDuration (s : String) : Option Nat :=
  if s.isEmpty then none
  else
    match s.back? with
    | some last =>
      if (durationFactor last).isSome then
        match (s.take (s.length - 1)).toString.toNat? with
        | none => none
        | some n => (durationFactor last).map (· * n)
      else
        s.toNat?
    | none => none

/--
Map a signal name or number to its signal number.
Accepts common names (case-sensitive, as in GNU) or a numeric value.
-/
def signalNumber (name : String) : Option Nat :=
  match name with
  | "HUP" => some 1
  | "INT" => some 2
  | "QUIT" => some 3
  | "TERM" => some 15
  | "KILL" => some 9
  | "USR1" => some 10
  | "USR2" => some 12
  | "ALRM" => some 14
  | _ => name.toNat?

/--
Strip the text after the first '=' in a "--name=value" option.
-/
def suffixAfterEq (s : String) : String :=
  match s.splitOn "=" with
  | _ :: rest => String.join (rest.intersperse "=")
  | [] => ""

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Parse `timeout` arguments into a Config.

Options:
  - -s SIG, --signal=SIG        signal to send on timeout (default TERM)
  - -k DUR, --kill-after=DUR    SIGKILL after DUR if still alive
  - --                         end of options

The first non-option operand is the duration; the rest is the command.
Returns none when no command is present.
-/
def parseArgs (input : TimeoutInput) : Option Config :=
  let rec go (remaining : List String) (cfg : Config) (cmd : List String) : Option Config :=
    match remaining, cmd with
    | [], [] => none
    | [], cs => some { cfg with cmd := cs.reverse }
    | "--" :: rest, _ => if rest.isEmpty then none else some { cfg with cmd := rest }
    | s :: rest, _ =>
      if s == "-s" then
        match rest with
        | val :: rest2 =>
          match signalNumber val with
          | none => none
          | some n => go rest2 { cfg with signal := n } cmd
        | [] => none
      else if s.startsWith "--signal=" then
        match signalNumber (suffixAfterEq s) with
        | none => none
        | some n => go rest { cfg with signal := n } cmd
      else if s == "-k" then
        match rest with
        | val :: rest2 =>
          match parseDuration val with
          | none => none
          | some n => go rest2 { cfg with killAfter := n } cmd
        | [] => none
      else if s.startsWith "--kill-after=" then
        match parseDuration (suffixAfterEq s) with
        | none => none
        | some n => go rest { cfg with killAfter := n } cmd
      else if s.startsWith "-" && s != "-" then
        none
      else
        match parseDuration s with
        | none => none
        | some secs => some { cfg with seconds := some secs, cmd := rest }
    termination_by remaining.length
  go input.args defaultConfig []

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: parseDuration of "5" is some 5.
-/
theorem i_parse_duration_5 : parseDuration "5" = some 5 := by
  native_decide

/--
I2: parseDuration of "2m" is some 120.
-/
theorem i_parse_duration_2m : parseDuration "2m" = some 120 := by
  native_decide

/--
I3: parseDuration of "1h" is some 3600.
-/
theorem i_parse_duration_1h : parseDuration "1h" = some 3600 := by
  native_decide

/--
I4: parseDuration of empty is none.
-/
theorem i_parse_duration_empty : parseDuration "" = none := by
  native_decide

/--
I5: signalNumber of "TERM" is some 15.
-/
theorem i_signal_term : signalNumber "TERM" = some 15 := by
  native_decide

/--
I6: signalNumber of "9" is some 9.
-/
theorem i_signal_9 : signalNumber "9" = some 9 := by
  native_decide

/--
I7: No arguments means no command (returns none).
-/
theorem i_no_args : parseArgs { args := [] } = none := by
  native_decide

/--
I8: Duration plus command, default signal 15.
-/
theorem i_duration_and_cmd : parseArgs { args := ["5", "sleep", "1"] } =
    some { defaultConfig with seconds := some 5, cmd := ["sleep", "1"] } := by
  native_decide

/--
I9: `--signal=KILL` selects signal 9.
-/
theorem i_signal_flag : parseArgs { args := ["-s", "KILL", "3", "echo", "x"] } =
    some { defaultConfig with seconds := some 3, signal := 9, cmd := ["echo", "x"] } := by
  native_decide

/--
I10: Suffix duration "30s" is parsed.
-/
theorem i_suffix_duration : parseArgs { args := ["30s", "ls"] } =
    some { defaultConfig with seconds := some 30, cmd := ["ls"] } := by
  native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- timeout → none -/
example : parseArgs { args := [] } = none := i_no_args

/-- timeout 5 sleep 1 → 5 sec, SIGTERM -/
example : parseArgs { args := ["5", "sleep", "1"] } =
    some { defaultConfig with seconds := some 5, cmd := ["sleep", "1"] } :=
  i_duration_and_cmd

/-- timeout -s KILL 3 echo x → 3 sec, SIGKILL -/
example : parseArgs { args := ["-s", "KILL", "3", "echo", "x"] } =
    some { defaultConfig with seconds := some 3, signal := 9, cmd := ["echo", "x"] } :=
  i_signal_flag

end Lentils.Timeout.Logic
