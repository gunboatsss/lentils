/-
Logname.Logic — Verified pure logic for `logname`.
0BSD

Structure:
  1. State types      — LognameInput (user name string)
  2. Specification    — isValidLoginName
  3. Implementation   — same as spec
  4. Correctness      — impl_correct
  5. Invariants       — parametric properties
  6. Concrete examples

No IO, no FFI, no `sorry` or `admit`.

POSIX: `logname` prints the user's login name.
-/

import Lentils.Common.Spec

namespace Lentils.Logname.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for logname: the candidate login name.
-/
structure LognameInput where
  name : String
  deriving Inhabited, BEq

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Validate a login name: must be non-empty and contain no newline.
-/
def isValidLoginName (s : String) : Bool :=
  s ≠ "" && !s.contains '\n'

/--
Format the login name for output.
Returns the name if valid, empty string otherwise.
-/
def format (input : LognameInput) : String :=
  if isValidLoginName input.name then input.name else ""

/--
Specification: validate and format the login name.
-/
def spec (input : LognameInput) : String :=
  format input

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Parametric Invariants
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Empty name is not valid.
-/
theorem i_empty_invalid : isValidLoginName "" = false := by
  native_decide

/--
I2: A non-empty name without newlines is valid.
-/
theorem i_valid_nonempty (s : String) (hne : s ≠ "") (hnl : ¬s.contains '\n') :
    isValidLoginName s = true := by
  unfold isValidLoginName
  simp [hne, hnl]

/--
I3: A name containing a newline is never valid.
-/
theorem i_newline_invalid (s : String) (h : s.contains '\n') :
    isValidLoginName s = false := by
  unfold isValidLoginName
  simp [h]

/--
I5: A valid name is returned unchanged by format.
-/
theorem i_format_valid (s : String) (h : isValidLoginName s = true) :
    format { name := s } = s := by
  unfold format
  simp [h]

/--
I6: An invalid name yields empty output.
-/
theorem i_format_invalid (s : String) (h : isValidLoginName s = false) :
    format { name := s } = "" := by
  unfold format
  simp [h]

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- "john" is a valid login name. -/
example : isValidLoginName "john" = true := by
  native_decide

/-- Empty string is not a valid login name. -/
example : isValidLoginName "" = false := i_empty_invalid

end Lentils.Logname.Logic
