/-
Realpath.Logic — Verified pure logic for `realpath`. 0BSD

Spec-first methodology.
-/

import Lentils.Common.Spec

namespace Lentils.Realpath.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Full input state for realpath: raw command-line arguments.
-/
structure RealpathInput where
  args : List String
  deriving Inhabited, BEq

/--
Default input: empty args.
-/
def defaultInput : RealpathInput := { args := [] }

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Validate that a path argument is provided.
Returns the first argument, or none if no arguments.
-/
def getPath (args : List String) : Option String :=
  args.head?

/--
Spec: get the path from args.
-/
def spec (input : RealpathInput) : Option String :=
  getPath input.args

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Empty args returns none.
-/
theorem i_empty : spec defaultInput = none := by
  native_decide

/--
I2: First arg is the path.
-/
theorem i_first_arg (path : String) : spec { args := [path] } = some path := by
  simp [spec, getPath]

/--
I4: Second arg is ignored.
-/
theorem i_second_ignored (path extra : String) :
    spec { args := [path, extra] } = some path := by
  simp [spec, getPath]

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- getPath returns the first argument -/
example : getPath ["/some/path"] = some "/some/path" := by native_decide

/-- getPath on empty args returns none -/
example : getPath [] = none := by native_decide

end Lentils.Realpath.Logic
