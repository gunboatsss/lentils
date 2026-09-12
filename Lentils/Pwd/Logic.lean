/-
Pwd.Logic — Verified pure logic for `pwd`.
0BSD

POSIX.1-2017 §pwd: prints the current working directory.

Structure:
  1. State types      — PwdInput (path from getcwd)
  2. Specification    — format: path ++ "\n"
  3. Correctness      — theorem: impl = spec
  4. Invariants       — parametric properties over all paths
  5. Concrete examples — derived corollaries

No IO, no FFI, no `sorry` or `admit`.
-/

import Lentils.Common.Spec

namespace Lentils.Pwd.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for pwd. The path comes from the OS via getcwd.
-/
structure PwdInput where
  path : String
  deriving Inhabited, BEq, Repr

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Format the working directory for output.
Appends a newline to the path.
-/
def format (input : PwdInput) : String :=
  input.path ++ "\n"

/--
Exit code of `pwd` on success. Always 0.
-/
def exitCode : UInt32 := 0

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Format appends a newline to the given path.
-/
theorem i_appends_newline (path : String) : format { path := path } = path ++ "\n" := rfl

/--
I2: Format is injective: if format p1 = format p2 then p1 = p2.
-/
theorem i_injective (p1 p2 : String) (h : format { path := p1 } = format { path := p2 }) : p1 = p2 := by
  simpa [format] using h

/--
I4: The output is always non-empty (at least the newline).
-/
theorem i_output_nonempty (path : String) : format { path := path } ≠ "" := by
  intro h
  have : path ++ "\n" ≠ "" := by
    intro h2
    have : "\n" = "" := by
      simpa using h2
    have hn : "\n" ≠ "" := by decide
    exact hn this
  apply this
  simpa [format] using h

/--
I6: Exit code is always 0.
-/
theorem i_exit_success : exitCode = 0 := rfl

/--
I7: Length of output = length of path + 1 (for the newline).
-/
theorem i_output_length (path : String) :
    (format { path := path }).length = path.length + 1 := by
  simp [format, String.length]

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- pwd in root → "/\n" -/
example : format { path := "/" } = "/\n" := rfl

/-- pwd in /home/user → "/home/user\n" -/
example : format { path := "/home/user" } = "/home/user\n" := rfl

end Lentils.Pwd.Logic
