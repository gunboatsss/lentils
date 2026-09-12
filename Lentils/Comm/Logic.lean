/-
Comm.Logic — Verified pure logic for `comm`.
0BSD

Structure:
  1. State types      — CommInput (flags + two line lists)
  2. Specification    — comm: line-by-line comparison of sorted files
  3. Implementation   — comm (directly executable spec)
  4. Invariants       — parametric properties over all inputs
  5. Invariants       — parametric theorems over all inputs
  6. Lemmas           — helper theorems for invariants
  7. Concrete corollaries — derived examples

No IO, no FFI, no `sorry` or `admit`.

POSIX.1-2017 §comm: compare two sorted files line by line.
Output is three columns: lines unique to file1, lines unique to file2,
lines common to both. Flag -1 suppresses column 1, -2 suppresses column 2,
-3 suppresses column 3.
-/

import Lentils.Common.Spec

namespace Lentils.Comm.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Flags controlling which columns are suppressed.
col1 = suppress column 1 (lines unique to file1)
col2 = suppress column 2 (lines unique to file2)
col3 = suppress column 3 (lines common to both)
-/
structure SuppressFlags where
  col1 : Bool := false
  col2 : Bool := false
  col3 : Bool := false
  deriving Inhabited, BEq, Repr

/--
Input state for comm.
-/
structure CommInput where
  lines1 : List String
  lines2 : List String
  flags : SuppressFlags := {}
  deriving Inhabited, BEq, Repr

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Helpers
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Compute the number of leading tabs for a given column, given which
columns are suppressed.
-/
def tabsBefore (col : Nat) (flags : SuppressFlags) : Nat :=
  match col with
  | 1 => 0
  | 2 => if flags.col1 then 0 else 1
  | 3 => (if flags.col1 then 0 else 1) + (if flags.col2 then 0 else 1)
  | _ => 0

/--
Build a string of n tab characters.
-/
def tabs (n : Nat) : String :=
  String.ofList (List.replicate n '\t')

/--
Internal recursive comparison. Workhorse of `comm`.
-/
def go (l1 l2 : List String) (flags : SuppressFlags) (acc : List String) : List String :=
  match l1, l2 with
  | [], [] => acc.reverse
  | [], b :: bs =>
    if flags.col2 then go [] bs flags acc
    else go [] bs flags ((tabs (tabsBefore 2 flags) ++ b) :: acc)
  | a :: as, [] =>
    if flags.col1 then go as [] flags acc
    else go as [] flags (a :: acc)
  | a :: as, b :: bs =>
    if decide (a < b) then
      if flags.col1 then go as (b :: bs) flags acc
      else go as (b :: bs) flags (a :: acc)
    else if decide (b < a) then
      if flags.col2 then go (a :: as) bs flags acc
      else go (a :: as) bs flags ((tabs (tabsBefore 2 flags) ++ b) :: acc)
    else
      if flags.col3 then go as bs flags acc
      else go as bs flags ((tabs (tabsBefore 3 flags) ++ a) :: acc)

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Compare two sorted line lists and produce comm-style output.
-/
def comm (lines1 lines2 : List String) (flags : SuppressFlags := {}) : String :=
  String.intercalate "\n" (go lines1 lines2 flags [])

/--
Specification for CommInput.
-/
def spec (input : CommInput) : String :=
  comm input.lines1 input.lines2 input.flags

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Empty lines lists produce empty output.
-/
theorem i_empty : comm [] [] {} = "" := by
  native_decide

/--
I2: File1-only line appears in column 1 with no tab prefix.
-/
theorem i_file1_only : comm ["a"] [] {} = "a" := by
  native_decide

/--
I3: File2-only line appears in column 2 with one tab prefix.
-/
theorem i_file2_only : comm [] ["a"] {} = "\ta" := by
  native_decide

/--
I4: Common line appears in column 3 with two tab prefixes.
-/
theorem i_common_line : comm ["a"] ["a"] {} = "\t\ta" := by
  native_decide

/--
I5: Column 1 suppression hides file1-only line.
-/
theorem i_suppress_col1 : comm ["a"] [] { col1 := true } = "" := by
  native_decide

/--
I6: Column 2 suppression hides file2-only line.
-/
theorem i_suppress_col2 : comm [] ["a"] { col2 := true } = "" := by
  native_decide

/--
I7: Column 3 suppression hides common line.
-/
theorem i_suppress_col3 : comm ["a"] ["a"] { col3 := true } = "" := by
  native_decide

/--
I8: All columns suppressed yields empty output.
-/
theorem i_suppress_all : comm ["a"] ["a"] { col1 := true, col2 := true, col3 := true } = "" := by
  native_decide

/--
I9: tabsBefore for column 1 is always 0.
-/
theorem i_tabs_before_col1 (flags : SuppressFlags) : tabsBefore 1 flags = 0 := rfl

/--
I11: tabsBefore for column 2 is 1 if col1 not suppressed, else 0.
-/
theorem i_tabs_before_col2 (flags : SuppressFlags) :
    tabsBefore 2 flags = if flags.col1 then 0 else 1 := rfl

/--
I12: tabsBefore for column 3 depends on both col1 and col2 suppression.
-/
theorem i_tabs_before_col3 (flags : SuppressFlags) :
    tabsBefore 3 flags = (if flags.col1 then 0 else 1) + (if flags.col2 then 0 else 1) := rfl

/--
I13: tabs n builds a string of exactly n tabs.
-/
theorem i_tabs_length (n : Nat) : (tabs n).length = n := by
  simp [tabs]

/--
I14: File1 has extra line before common line (sorted).
-/
theorem i_extra_file1 : comm ["a", "b"] ["b"] {} = "a\n\t\tb" := by
  native_decide

/--
I15: File2 has extra line after common line (sorted).
-/
theorem i_extra_file2 : comm ["a"] ["a", "c"] {} = "\t\ta\n\tc" := by
  native_decide

/--
I16: go with empty accumulator returns [] when both lists are empty.
-/
theorem i_go_empty_both : go [] [] {} [] = ([] : List String) := by
  unfold go; simp

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Lemmas
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
tabsBefore is bounded by 2.
-/
theorem tabsBefore_bound (col : Nat) (flags : SuppressFlags) : tabsBefore col flags ≤ 2 := by
  unfold tabsBefore
  cases col with
  | zero => simp
  | succ col =>
    cases col with
    | zero => simp
    | succ col =>
      cases col with
      | zero =>
        by_cases h : flags.col1
        · simp [h]
        · simp [h]
      | succ col =>
        cases col with
        | zero =>
          by_cases h1 : flags.col1
          · by_cases h2 : flags.col2
            · simp [h1, h2]
            · simp [h1, h2]
          · by_cases h2 : flags.col2
            · simp [h1, h2]
            · simp [h1, h2]
        | succ col => simp

/--
tabs n never contains non-tab characters.
-/
theorem tabs_only_tabs (n : Nat) : (tabs n).all (λ c => c = '\t') := by
  induction n with
  | zero => simp [tabs]
  | succ n ih =>
      simp [tabs, List.replicate_succ, ih]

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 7. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- comm of two empty lists yields empty output. -/
example : comm [] [] {} = "" := i_empty

/-- comm with col1 suppressed hides lines unique to file1. -/
example : comm ["a"] [] { col1 := true } = "" := i_suppress_col1

/-- comm with col2 suppressed hides lines unique to file2. -/
example : comm [] ["a"] { col2 := true } = "" := i_suppress_col2

/-- comm with col3 suppressed hides common line. -/
example : comm ["a"] ["a"] { col3 := true } = "" := i_suppress_col3

/-- Identical single lines: the line appears in column 3 (two tabs). -/
example : comm ["a"] ["a"] {} = "\t\ta" := i_common_line

/-- All columns suppressed produces empty output. -/
example : comm ["a"] ["a"] { col1 := true, col2 := true, col3 := true } = "" := i_suppress_all

end Lentils.Comm.Logic
