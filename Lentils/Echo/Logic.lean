/-
Echo.Logic — Verified pure logic for `echo`.
0BSD

Structure:
  1. State types      — EchoInput (flags + args)
  2. Specification    — format: the formal "what" and "how" (directly executable)
  3. Correctness      — theorem: the implementation is the specification
  4. Invariants       — parametric properties over all inputs
  5. Concrete examples — derived corollaries

No IO, no FFI, no `sorry` or `admit`.

POSIX.1-2017 §echo: writes args separated by spaces, followed by newline.
-n is implementation-defined. We use BSD semantics (strip leading -n, suppress newline).
Escape sequences are NOT processed (literal text).
-/

import Lentils.Common.Spec

namespace Lentils.Echo.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for echo.
-/
structure EchoInput where
  suppressNewline : Bool
  args : List String
  deriving Inhabited, BEq, Repr

def defaultInput : EchoInput := { suppressNewline := false, args := [] }

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Helpers
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Intercalate a separator between list elements.
Uses structural recursion for straightforward induction.
-/
def intercalate (sep : String) : List String → String
  | []      => ""
  | [x]     => x
  | x :: xs => x ++ sep ++ intercalate sep xs

/--
Consume all leading "-n" flags. Returns (remaining, anyStripped).
-/
def stripLeadingN : List String → List String × Bool
  | "-n" :: rest => 
      let (remaining, _) := stripLeadingN rest
      (remaining, true)
  | args => (args, false)

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
The echo specification. Since the spec is directly executable,
the implementation IS the specification.
-/
def format (input : EchoInput) : String :=
  let (remaining, flagSuppress) := stripLeadingN input.args
  let joined := intercalate " " remaining
  let suppress := input.suppressNewline || flagSuppress
  if suppress then joined else joined ++ "\n"

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Empty input with default flags → just a newline.
-/
theorem i_empty : format defaultInput = "\n" := rfl

/--
I2: For any single arg s ≠ "-n", output = s ++ "\n".
Parametric over all such strings.
-/
theorem i_unary (s : String) (h : s ≠ "-n") :
    format { suppressNewline := false, args := [s] } = s ++ "\n" := by
  unfold format stripLeadingN
  simp [h]
  rfl

/--
I3: For any two args (first ≠ "-n"), output = s1 ++ " " ++ s2 ++ "\n".
Parametric.
-/
theorem i_binary (s1 s2 : String) (h1 : s1 ≠ "-n") :
    format { suppressNewline := false, args := [s1, s2] } = s1 ++ " " ++ s2 ++ "\n" := by
  unfold format stripLeadingN
  simp [h1]
  rfl

/--
I4: For any three args (first ≠ "-n"), output = s1 ++ " " ++ s2 ++ " " ++ s3 ++ "\n".
Parametric.
-/
theorem i_ternary (s1 s2 s3 : String) (h1 : s1 ≠ "-n") :
    format { suppressNewline := false, args := [s1, s2, s3] } = s1 ++ " " ++ s2 ++ " " ++ s3 ++ "\n" := by
  unfold format stripLeadingN
  simp [h1]
  have h_inter : intercalate " " [s1, s2, s3] = s1 ++ " " ++ s2 ++ " " ++ s3 := by
    calc
      intercalate " " [s1, s2, s3] = s1 ++ " " ++ intercalate " " [s2, s3] := rfl
      _ = s1 ++ " " ++ (s2 ++ " " ++ s3) := rfl
      _ = s1 ++ " " ++ s2 ++ " " ++ s3 := by simp [String.append_assoc]
  rw [h_inter]

/--
I5: With suppressNewline=true and empty args, output is empty.
-/
theorem i_suppress_empty :
    format { suppressNewline := true, args := [] } = "" := by
  unfold format stripLeadingN intercalate; rfl

/--
I6: Leading "-n" is consumed. Any single arg "-n" produces empty output.
-/
theorem i_leading_n_alone :
    format { suppressNewline := false, args := ["-n"] } = "" := by
  unfold format stripLeadingN intercalate; rfl

/--
I7: Multiple leading "-n" flags are all consumed (idempotence of -n stripping).
-/
theorem i_multiple_n (args : List String) :
    format { suppressNewline := false, args := ["-n", "-n"] ++ args } =
    format { suppressNewline := false, args := ["-n"] ++ args } := by
  unfold format
  have h_strip : stripLeadingN (["-n", "-n"] ++ args) = stripLeadingN (["-n"] ++ args) := by
    simp [stripLeadingN]
  rw [h_strip]

/--
I8: "-n" not in first position is preserved literally.
-/
theorem i_n_nonfirst (s : String) (h : s ≠ "-n") :
    format { suppressNewline := false, args := [s, "-n"] } = s ++ " " ++ "-n" ++ "\n" := by
  unfold format stripLeadingN
  simp [h, intercalate]

/--
I9: Exit code is always 0 for echo.
-/
def exitCode : ExitCode := exitSuccess

theorem i_exit_success : exitCode = exitSuccess := rfl

/--
I12: The format function can be decomposed: step 1 = stripLeadingN, step 2 = intercalate, step 3 = newline decision.
This is a structural lemma useful for compositional proofs.
-/
theorem i_decompose (input : EchoInput) :
    format input =
      let (remaining, flagSuppress) := stripLeadingN input.args
      let joined := intercalate " " remaining
      if input.suppressNewline || flagSuppress then joined else joined ++ "\n"
    := rfl

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. StripLeadingN Lemmas
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
stripLeadingN on empty list returns ([], false).
-/
theorem stripN_empty : stripLeadingN [] = ([], false) := rfl

/--
stripLeadingN on ["-n"] returns ([], true).
-/
theorem stripN_one : stripLeadingN ["-n"] = ([], true) := rfl

/--
stripLeadingN on multiple "-n" consumes all.
-/
theorem stripN_multiple (n : Nat) :
    stripLeadingN (List.replicate n "-n" ++ ["hello"]) = (["hello"], decide (0 < n)) := by
  induction n with
  | zero => rfl
  | succ n ih =>
      simp [List.replicate_succ, stripLeadingN, ih]

/--
stripLeadingN only removes elements, so the result is no longer than input.
-/
theorem stripN_preserves_order (args : List String) :
    (stripLeadingN args).1.length ≤ args.length := by
  induction args with
  | nil => simp [stripLeadingN]
  | cons a as ih =>
      by_cases h : a = "-n"
      · simp [stripLeadingN, h]
        exact Nat.le_trans ih (Nat.le_succ _)
      · simp [stripLeadingN, h]

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Intercalate Lemmas
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Empty list.
-/
theorem intercalate_nil (sep : String) : intercalate sep [] = "" := rfl

/--
Singleton list.
-/
theorem intercalate_single (sep : String) (s : String) : intercalate sep [s] = s := rfl

/--
Two-element list.
-/
theorem intercalate_pair (sep : String) (s1 s2 : String) : intercalate sep [s1, s2] = s1 ++ sep ++ s2 := rfl

/--
Three-element list.
-/
theorem intercalate_triple (sep : String) (s1 s2 s3 : String) :
    intercalate sep [s1, s2, s3] = s1 ++ sep ++ s2 ++ sep ++ s3 := by
  simp [intercalate, String.append_assoc]

/--
Intercalate into a non-empty list where all elements are non-empty
produces a non-empty string.
-/
theorem intercalate_nonempty (sep : String) (xs : List String)
    (hne : xs ≠ []) (h_all : ∀ s ∈ xs, s ≠ "") :
    intercalate sep xs ≠ "" := by
  cases xs with
  | nil => simp at hne
  | cons x xs =>
      have hx : x ≠ "" := h_all x (by simp)
      cases xs with
      | nil =>
          simp [intercalate, hx]
      | cons y ys =>
          simp [intercalate, hx]

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 7. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- echo → newline only -/
example : format defaultInput = "\n" := i_empty

/-- echo hello → "hello\n" -/
example : format { suppressNewline := false, args := ["hello"] } = "hello\n" :=
  i_unary "hello" (by decide)

/-- echo hello world → "hello world\n" -/
example : format { suppressNewline := false, args := ["hello", "world"] } = "hello world\n" :=
  i_binary "hello" "world" (by decide)

/-- echo -n hello → "hello" -/
example : format { suppressNewline := true, args := ["hello"] } = "hello" := by
  unfold format stripLeadingN intercalate; rfl

/-- echo a b c → "a b c\n" -/
example : format { suppressNewline := false, args := ["a", "b", "c"] } = "a b c\n" :=
  i_ternary "a" "b" "c" (by decide)

/-- echo hello -n → "hello -n\n" (literal -n when not first) -/
example : format { suppressNewline := false, args := ["hello", "-n"] } = "hello" ++ " " ++ "-n" ++ "\n" :=
  i_n_nonfirst "hello" (by decide)

end Lentils.Echo.Logic
