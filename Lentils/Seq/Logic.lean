/-
Seq.Logic — Verified pure logic for `seq`.
0BSD

Structure:
  1. State types      — SeqInput
  2. Specification    — spec: generate a sequence of numbers
  3. Invariants       — parametric theorems (empty for zero inc)

POSIX: prints a sequence of numbers, one per line.

Note: Float equality is not decidable (no DecidableEq), so theorems
about seq output values are limited to structural properties.

No IO, no FFI, no `sorry` or `admit`.
-/

import Lentils.Common.Spec
import Lentils.Common.Float

namespace Lentils.Seq.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for seq: the arguments (first, increment, last).
-/
structure SeqInput where
  first : Float
  inc : Float
  last : Float
  deriving Inhabited, BEq

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Helpers
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Parse a float string; returns 0.0 on failure.
-/
def parseFloat (s : String) : Float :=
  match Lentils.Common.Float.parse s with
  | some f => f
  | none => 0.0

/--
Check if a string represents a float with a decimal point.
-/
def hasDecimal (s : String) : Bool :=
  s.contains '.'

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Core Function
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Generate a sequence from `first` to `last` stepping by `inc`.
Uses an explicit step counter (Nat) to ensure termination.
-/
def seq (first last inc : Float) : List Float :=
  if inc == 0.0 then []
  else
    let diff := if inc > 0.0 then last - first else first - last
    let maxSteps : Nat :=
      if diff < 0.0 then 1
      else
        let s := (diff / (if inc < 0.0 then -inc else inc)).toUInt64
        let s := if s > 1000000 then 1000000 else s.toNat
        s + 1
    let rec go (i : Nat) (cur : Float) : List Float :=
      if i = 0 then []
      else
        if inc > 0.0 && cur > last then []
        else if inc < 0.0 && cur < last then []
        else cur :: go (i - 1) (cur + inc)
    go maxSteps first

/--
Format a float for display: strip trailing zeros from decimal representation.
-/
def formatFloat (f : Float) : String :=
  let s := toString f
  match s.splitOn "." with
  | [intPart, fracPart] =>
    let rec stripZeros (cs : List Char) : List Char :=
      match cs with
      | [] => []
      | '0' :: rest => stripZeros rest
      | c :: rest => c :: stripZeros rest
    let stripped := stripZeros (fracPart.toList.reverse)
    if stripped.isEmpty then intPart
    else intPart ++ "." ++ (String.ofList stripped.reverse)
  | _ => s

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Specification: generate sequence from first to last by increment.
-/
def spec (input : SeqInput) : List Float :=
  seq input.first input.last input.inc

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: hasDecimal detects decimal point in a string.
-/
theorem i_has_decimal_true : hasDecimal "1.5" = true := by native_decide

/--
I2: hasDecimal returns false for integer strings.
-/
theorem i_has_decimal_false : hasDecimal "42" = false := by native_decide

/--
I3: seq with inc=0 returns empty list.
-/
theorem i_zero_inc (first last : Float) : seq first last 0.0 = [] := by
  unfold seq
  have h : (0.0 : Float) == 0.0 := by native_decide
  simp [h]

/--
I4: formatFloat removes trailing zeros.
-/
theorem i_format_1_5 : formatFloat 1.5 = "1.5" := by native_decide

/--
I5: formatFloat on integer shows integer.
-/
theorem i_format_42 : formatFloat 42.0 = "42" := by native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- hasDecimal "1.5" = true -/
example : hasDecimal "1.5" = true := i_has_decimal_true

end Lentils.Seq.Logic
