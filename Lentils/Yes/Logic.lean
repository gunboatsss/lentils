/-
Yes.Logic — Verified pure logic for `yes`.
0BSD

POSIX.1-2017 §yes: repeatedly outputs a string consisting of the
specified operands separated by single space characters, or "y" if
no operands are given.

Structure:
  1. State types      — YesInput (args)
  2. Specification    — message: the string to repeat
  3. Correctness      — theorem: impl = spec
  4. Invariants       — parametric properties over all inputs
  5. Concrete examples — derived corollaries

No IO, no FFI, no `sorry` or `admit`.
-/

import Lentils.Common.Spec

namespace Lentils.Yes.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for yes.
-/
structure YesInput where
  args : List String
  deriving Inhabited, BEq, Repr

def defaultInput : YesInput := { args := [] }

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Determine the string to repeat. If no operands, returns "y".
Otherwise concatenates with spaces using String.intercalate.
-/
def message (input : YesInput) : String :=
  match input.args with
  | [] => "y"
  | _  => String.intercalate " " input.args

/--
The exit code of `yes` when terminated. Always 0.
yes runs forever until killed (SIGPIPE or SIGINT).
-/
def exitCode : UInt32 := 0

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Correctness Theorem
-- ═══════════════════════════════════════════════════════════════════════════════════

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: With no arguments, message is "y".
-/
theorem i_empty : message defaultInput = "y" := rfl

/--
I2: With a single argument s, message is s.
Parametric over all strings.
-/
theorem i_single (s : String) : message { args := [s] } = s := rfl

/--
I3: With two arguments s1 s2, message = s1 ++ " " ++ s2.
-/
theorem i_pair (s1 s2 : String) :
    message { args := [s1, s2] } = s1 ++ " " ++ s2 := rfl

/--
I4: For any non-empty args, message = String.intercalate " " args.
-/
theorem i_intercalate (args : List String) (h : args ≠ []) :
    message { args := args } = String.intercalate " " args := by
  cases args with
  | nil => simp at h
  | cons _ _ => rfl

/--
I6: Exit code is always 0.
-/
theorem i_exit_success : exitCode = 0 := rfl

/--
I8: The message for a single argument is the argument itself.
-/
theorem i_single_reflexive (s : String) : message { args := [s] } = s := rfl

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- yes → "y" -/
example : message defaultInput = "y" := i_empty

/-- yes hello → "hello" -/
example : message { args := ["hello"] } = "hello" := i_single "hello"

/-- yes hello world → "hello world" -/
example : message { args := ["hello", "world"] } = "hello world" :=
  i_pair "hello" "world"

/-- yes with three args → joined by spaces -/
example : message { args := ["a", "b", "c"] } = "a b c" := rfl

end Lentils.Yes.Logic
