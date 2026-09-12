/-
False.Logic — Verified pure logic for `false`.
0BSD

POSIX.1-2017 §false: always returns exit code 1, ignoring all operands.

Structure:
  1. State types      — FalseInput (flags + args)
  2. Specification    — exitCode: always 1
  3. Correctness      — theorem: impl = spec (trivially)
  4. Invariants       — parametric properties over all inputs
  5. Concrete examples — derived corollaries

No IO, no FFI, no `sorry` or `admit`.
-/

import Lentils.Common.Spec

namespace Lentils.False.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for false. All arguments are ignored.
-/
structure FalseInput where
  args : List String
  deriving Inhabited, BEq, Repr

def defaultInput : FalseInput := { args := [] }

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
The exit code of `false`. Always 1 regardless of input.
-/
def exitCode (input : FalseInput) : UInt32 := 1

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Exit code is always 1, regardless of input.
-/
theorem i_exit_failure (input : FalseInput) : exitCode input = 1 := rfl

/--
I2: Exit code is non-zero for all inputs.
-/
theorem i_exit_nonzero (input : FalseInput) : exitCode input ≠ 0 := by
  simp [exitCode]

/--
I4: Arguments are ignored — any args produce the same result as no args.
-/
theorem i_args_ignored (args : List String) :
    exitCode { args := args } = exitCode { args := [] } := rfl

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- false → exit code 1 -/
example : exitCode defaultInput = 1 := i_exit_failure defaultInput

/-- false with args → exit code 1 -/
example : exitCode { args := ["hello", "world"] } = 1 :=
  i_exit_failure { args := ["hello", "world"] }

end Lentils.False.Logic
