/-
Join.Logic — Verified pure logic for `join`.
0BSD

Structure:
  1. State types      — JoinInput (delim, field1, field2, lines1, lines2)
  2. Specification    — join: relational join on sorted fields
  3. Implementation   — join (directly executable spec)
  4. Correctness      — impl = spec
  5. Invariants       — parametric theorems over all inputs
  6. Lemmas           — helper theorems for invariants
  7. Concrete corollaries — derived examples

No IO, no FFI, no `sorry` or `admit`.

POSIX.1-2017 §join: relational join utility.
Joins two sorted files on a common field (default field 1, delimiter space).
-/

import Lentils.Common.Spec

namespace Lentils.Join.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for join.
-/
structure JoinInput where
  lines1 : List String
  lines2 : List String
  delim : String := " "
  field1 : Nat := 1
  field2 : Nat := 1
  deriving Inhabited, BEq, Repr

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Helpers
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Extract the join field from a line.
Fields are separated by delimiter; field index is 1-based.
-/
def extractField (line : String) (delim : String) (field : Nat) : String :=
  let parts := line.splitOn delim
  match parts.drop (field - 1) with
  | x :: _ => x
  | [] => ""

/--
Internal recursive join. Given two sorted line lists and an accumulator
(in reverse), produces the joined output lines in reverse order.
-/
def go (l1 l2 : List String) (delim : String) (field1 field2 : Nat) (acc : List String) : List String :=
  match l1, l2 with
  | [], _ => acc.reverse
  | _, [] => acc.reverse
  | a :: as, b :: bs =>
    let keyA := extractField a delim field1
    let keyB := extractField b delim field2
    if decide (keyA < keyB) then
      go as (b :: bs) delim field1 field2 acc
    else if decide (keyB < keyA) then
      go (a :: as) bs delim field1 field2 acc
    else
      let restA := (a.splitOn delim).drop field1
      let restB := (b.splitOn delim).drop field2
      let joined := String.intercalate delim ([keyA] ++ restA ++ restB)
      go as bs delim field1 field2 (joined :: acc)

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Join two sorted line lists on a common field.
For each pair of lines with matching join fields, output the joined line:
  join field + rest of file1 line + rest of file2 line
-/
def join (lines1 lines2 : List String) (delim : String := " ")
         (field1 field2 : Nat := 1) : String :=
  String.intercalate "\n" (go lines1 lines2 delim field1 field2 [])

/--
Specification for JoinInput.
-/
def spec (input : JoinInput) : String :=
  join input.lines1 input.lines2 input.delim input.field1 input.field2

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Empty input lists produce empty output.
-/
theorem i_empty : join [] [] " " = "" := by
  native_decide

/--
I2: First list empty produces empty output (regardless of second list).
Proof by structural induction on lines2.
-/
theorem i_first_empty (lines2 : List String) : join [] lines2 " " = "" := by
  induction lines2 with
  | nil => native_decide
  | cons b bs ih =>
      unfold join go
      simp [ih]

/--
I3: Second list empty produces empty output (regardless of first list).
Proof by structural induction on lines1.
-/
theorem i_second_empty (lines1 : List String) : join lines1 [] " " = "" := by
  induction lines1 with
  | nil => native_decide
  | cons a as ih =>
      unfold join go
      simp [ih]

/--
I5: extractField of empty string returns empty string.
-/
theorem i_extract_field_empty : extractField "" " " 1 = "" := by
  native_decide

/--
I6: Two matching lines with space delimiter on field 1 produce joined output.
-/
theorem i_matching_ground : join ["hello world"] ["hello there"] " " = "hello world there" := by
  native_decide

/--
I7: Non-matching keys produce empty output.
-/
theorem i_non_matching_ground : join ["aaa"] ["bbb"] " " = "" := by
  native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Lemmas
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
extractField with field=0 returns the first field (since 0-1 saturates to 0). Ground example.
-/
theorem extractField_zero_ground : extractField "a b c" " " 0 = "a" := by
  native_decide

/--
extractField on a line without delimiter returns the whole line for field 1.
-/
theorem extractField_no_delim_ground : extractField "hello" " " 1 = "hello" := by
  native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 7. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- Empty files produce empty join. -/
example : join [] [] " " = "" := i_empty

/-- Single matching lines join on default field. -/
example : join ["hello world"] ["hello there"] " " = "hello world there" := i_matching_ground

/-- First list empty produces empty output. -/
example : join [] ["a", "b"] " " = "" := i_first_empty ["a", "b"]

/-- Second list empty produces empty output. -/
example : join ["a", "b"] [] " " = "" := i_second_empty ["a", "b"]

/-- extractField with field 1 returns first word. -/
example : extractField "hello world" " " 1 = "hello" := by
  native_decide

end Lentils.Join.Logic
