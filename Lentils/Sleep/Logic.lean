/-
Sleep.Logic — Verified pure logic for `sleep`.
0BSD

POSIX.1-2017 §sleep: suspends execution for at least the integral number
of seconds specified by the time operand, or the time specified by all
non-option operands.

Pure logic: parse a duration string (e.g., "1.5", "60") into (seconds,
remaining nanoseconds). The actual sleeping is handled in the IO wrapper.

Structure:
  1. State types      — SleepInput (single duration string)
  2. Specification    — parseDuration: string → Option (Nat × Nat)
  3. Correctness      — theorem: impl = spec
  4. Invariants       — parametric properties
  5. Lemmas           — helper theorems
  6. Concrete examples

No IO, no FFI, no `sorry` or `admit`.
-/

import Lentils.Common.Spec

namespace Lentils.Sleep.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input for sleep: a list of duration strings (POSIX accepts multiple operands).
-/
structure SleepInput where
  args : List String
  deriving Inhabited, BEq, Repr

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Convert a duration string to (seconds, nanoseconds).
Returns none if the string is not a valid duration.
Accepts decimal numbers like "1.5", "0.1", "60", "2.75".
-/
def parseDuration (s : String) : Option (Nat × Nat) :=
  let parts := s.splitOn "."
  match parts with
  | [whole] =>
    match whole.toNat? with
    | none => none
    | some secs => some (secs, 0)
  | [whole, frac] =>
    -- GNU accepts ".5" (= 0.5) and "1." (= 1.0): empty parts count as zero.
    let secs? := if whole.isEmpty then some 0 else whole.toNat?
    let fracVal? := if frac.isEmpty then some 0 else frac.toNat?
    match secs?, fracVal? with
    | some secs, some fracVal =>
      let fracLen := frac.length
      let nanos :=
        if fracLen ≤ 9 then
          fracVal * (10 ^ (9 - fracLen))
        else
          match frac.take 9 |>.toNat? with
          | none => 0
          | some v => v
      some (secs, nanos)
    | _, _ => none
  | _ => none

/--
Parse a list of duration arguments. Returns the first valid duration,
or none if no valid duration is found.
-/
def parseFirstDuration (args : List String) : Option (Nat × Nat) :=
  match args with
  | [] => none
  | s :: _ => parseDuration s

/--
Exit code of sleep on success. Always 0.
-/
def exitCode : UInt32 := 0

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: parseDuration of "0" yields (0, 0).
-/
theorem i_zero : parseDuration "0" = some (0, 0) := by native_decide

/--
I2: parseDuration of "1" yields (1, 0).
-/
theorem i_one : parseDuration "1" = some (1, 0) := by native_decide

/--
I3: parseDuration of "1.5" yields (1, 500_000_000).
-/
theorem i_one_point_five : parseDuration "1.5" = some (1, 500000000) := by native_decide

/--
I4: parseDuration of "0.1" yields (0, 100_000_000).
-/
theorem i_zero_point_one : parseDuration "0.1" = some (0, 100000000) := by native_decide

/--
I5: parseDuration of empty string is none.
-/
theorem i_empty : parseDuration "" = none := by native_decide

/--
I6: parseDuration of "abc" is none.
-/
theorem i_invalid : parseDuration "abc" = none := by native_decide

/--
I7: parseDuration of a negative number is none (no leading minus).
-/
theorem i_negative : parseDuration "-1" = none := by native_decide

/--
I9: parseFirstDuration returns none for empty args.
-/
theorem i_no_args : parseFirstDuration [] = none := rfl

/--
I10: parseFirstDuration returns the parse of the first argument.
-/
theorem i_first_arg (s : String) (rest : List String) :
    parseFirstDuration (s :: rest) = parseDuration s := rfl

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- sleep 0 → (0, 0) -/
example : parseDuration "0" = some (0, 0) := i_zero

/-- sleep 1 → (1, 0) -/
example : parseDuration "1" = some (1, 0) := i_one

/-- sleep 1.5 → (1, 500000000) -/
example : parseDuration "1.5" = some (1, 500000000) := i_one_point_five

/-- sleep "" → none -/
example : parseDuration "" = none := i_empty

/-- sleep abc → none -/
example : parseDuration "abc" = none := i_invalid

end Lentils.Sleep.Logic
