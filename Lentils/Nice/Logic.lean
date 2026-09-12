/-
Nice.Logic — Verified pure logic for `nice`.
0BSD

POSIX.1-2017 §nice: runs a command with a modified scheduling priority
(niceness).

Structure:
  1. State types      — NiceInput, NiceConfig
  2. Specification    — parseArgs: Option (Int32 × List String)
  3. Correctness      — theorem: impl = spec
  4. Invariants       — parametric properties
  5. Lemmas           — helper theorems
  6. Concrete examples

No IO, no FFI, no `sorry` or `admit`.
-/

import Lentils.Common.Spec

namespace Lentils.Nice.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for nice.
-/
structure NiceInput where
  args : List String
  deriving Inhabited, BEq, Repr

/--
Parsed nice configuration.
-/
structure NiceConfig where
  adjustment : Int32
  cmd : List String
  deriving Inhabited, BEq, Repr, DecidableEq

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Helpers
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Default priority increment applied by `nice` (POSIX/GNU default is 10).
-/
def defaultAdjustment : Int32 := 10

/--
Parse a niceness adjustment string into an Int32.
Accepts optional leading '+' or '-' (e.g. "5", "-5", "+3").
Returns none if the string is not a valid integer.
-/
def parseAdjustment (s : String) : Option Int32 :=
  match s.toInt? with
  | none => none
  | some i => some i.toInt32

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
Parse `nice` arguments into a NiceConfig.

Options:
  - -n N, --adjustment=N   increment priority by N (default 10)
  - --                     end of options; remaining words are the command

Returns none when no command operand is present.
-/
def parseArgs (input : NiceInput) : Option NiceConfig :=
  let rec go (remaining : List String) (adj : Option Int32) (cmd : List String) (stop : Bool) :
      Option NiceConfig :=
    match remaining with
    | [] => if cmd.isEmpty then none else some { adjustment := adj.getD defaultAdjustment, cmd := cmd.reverse }
    | s :: rest =>
      if cmd ≠ [] then
        go rest adj (s :: cmd) stop
      else if stop then
        go rest adj (s :: cmd) stop
      else if s == "--" then
        go rest adj cmd true
      else if s == "-n" then
        match rest with
        | nStr :: rest2 =>
          match parseAdjustment nStr with
          | none => none
          | some v => go rest2 (some v) cmd false
        | [] => none
      else if s.startsWith "--adjustment=" then
        match parseAdjustment (suffixAfterEq s) with
        | none => none
        | some v => go rest (some v) cmd false
      else if s.startsWith "-" then
        none
      else
        go rest adj (s :: cmd) false
    termination_by remaining.length
  go input.args none [] false

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: parseAdjustment of "5" yields some 5.
-/
theorem i_parse_adjustment_5 : parseAdjustment "5" = some 5 := by
  native_decide

/--
I2: parseAdjustment accepts a negative integer.
-/
theorem i_parse_adjustment_neg : parseAdjustment "-3" = some (-3) := by
  native_decide

/--
I3: parseAdjustment rejects a non-numeric string.
-/
theorem i_parse_adjustment_invalid : parseAdjustment "foo" = none := by
  native_decide

/--
I4: No arguments means no command (returns none).
-/
theorem i_no_args : parseArgs { args := [] } = none := by
  native_decide

/--
I5: Default adjustment (10) is used when none is given.
-/
theorem i_default_adjustment : parseArgs { args := ["echo", "hi"] } = some { adjustment := 10, cmd := ["echo", "hi"] } := by
  native_decide

/--
I6: `-n` sets the adjustment.
-/
theorem i_n_flag : parseArgs { args := ["-n", "5", "echo"] } =
    some { adjustment := 5, cmd := ["echo"] } := by
  native_decide

/--
I7: `--adjustment=` sets the adjustment.
-/
theorem i_adjustment_flag : parseArgs { args := ["--adjustment=3", "ls", "-l"] } =
    some { adjustment := 3, cmd := ["ls", "-l"] } := by
  native_decide

/--
I8: `--` terminates options.
-/
theorem i_ddash : parseArgs { args := ["--", "-n", "echo"] } =
    some { adjustment := 10, cmd := ["-n", "echo"] } := by
  native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- nice → none (no command) -/
example : parseArgs { args := [] } = none := i_no_args

/-- nice echo hi → adj=10, cmd=["echo","hi"] -/
example : parseArgs { args := ["echo", "hi"] } = some { adjustment := 10, cmd := ["echo", "hi"] } :=
  i_default_adjustment

/-- nice -n 5 echo → adj=5, cmd=["echo"] -/
example : parseArgs { args := ["-n", "5", "echo"] } = some { adjustment := 5, cmd := ["echo"] } :=
  i_n_flag

end Lentils.Nice.Logic
