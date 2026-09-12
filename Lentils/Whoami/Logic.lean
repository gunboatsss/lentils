/-
Whoami.Logic — Verified pure logic for `whoami`.
0BSD

Structure:
  1. State types      — WhoamiInput (user name)
  2. Specification    — isValidUserName (single implementation; no duplicate `impl` alias)
  3. Invariants       — parametric properties
  4. Concrete examples

No IO, no FFI, no `sorry` or `admit`.

POSIX: `whoami` prints the effective user name.
-/

import Lentils.Common.Spec

namespace Lentils.Whoami.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for whoami: the candidate user name.
-/
structure WhoamiInput where
  name : String
  deriving Inhabited, BEq

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Validate a user name: must be non-empty and contain no newline.
-/
def isValidUserName (s : String) : Bool :=
  s ≠ "" && !s.contains '\n'

/--
Format the user name for output.
Returns the name if valid, empty string otherwise.
-/
def format (input : WhoamiInput) : String :=
  if isValidUserName input.name then input.name else ""

/--
Specification: validate and format the user name.
-/
def spec (input : WhoamiInput) : String :=
  format input

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Parametric Invariants
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Empty name is not valid.
-/
theorem i_empty_invalid : isValidUserName "" = false := by
  native_decide

/--
I2: A non-empty name without newlines is valid.
-/
theorem i_valid_nonempty (s : String) (hne : s ≠ "") (hnl : ¬s.contains '\n') :
    isValidUserName s = true := by
  unfold isValidUserName
  simp [hne, hnl]

/--
I3: A name containing a newline is never valid.
-/
theorem i_newline_invalid (s : String) (h : s.contains '\n') :
    isValidUserName s = false := by
  unfold isValidUserName
  simp [h]

/--
I5: A valid name is returned unchanged by format.
-/
theorem i_format_valid (s : String) (h : isValidUserName s = true) :
    format { name := s } = s := by
  unfold format
  simp [h]

/--
I6: An invalid name yields empty output.
-/
theorem i_format_invalid (s : String) (h : isValidUserName s = false) :
    format { name := s } = "" := by
  unfold format
  simp [h]

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- "root" is a valid user name. -/
example : isValidUserName "root" = true := by
  native_decide

/-- Empty string is not a valid user name. -/
example : isValidUserName "" = false := i_empty_invalid

end Lentils.Whoami.Logic
