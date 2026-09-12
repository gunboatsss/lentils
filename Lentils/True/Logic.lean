/-
True.Logic — Verified pure logic for `true`.
0BSD

POSIX.1-2017 §true: always returns exit code 0, ignoring all operands.

Structure:
  1. State types      — TrueInput (flags + args)
  2. Specification    — exitCode: always 0
  3. Correctness      — theorem: impl = spec (trivially)
  4. Invariants       — parametric properties over all inputs
  5. Concrete examples — derived corollaries

No IO, no FFI, no `sorry` or `admit`.
-/

import Lentils.Common.Spec

namespace Lentils.True.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for true. All arguments are ignored.
-/
structure TrueInput where
  args : List String
  deriving Inhabited, BEq, Repr

def defaultInput : TrueInput := { args := [] }

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
The exit code of `true`. Always 0 regardless of input.
This is both the specification and the implementation.
-/
def exitCode (input : TrueInput) : UInt32 := 0

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Exit code is always 0, regardless of input.
-/
theorem i_exit_success (input : TrueInput) : exitCode input = 0 := rfl

/--
I3: Arguments are ignored — any args produce the same result as no args.
-/
theorem i_args_ignored (args : List String) :
    exitCode { args := args } = exitCode { args := [] } := rfl

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- true → exit code 0 -/
example : exitCode defaultInput = 0 := i_exit_success defaultInput

/-- true with args → exit code 0 -/
example : exitCode { args := ["hello", "world"] } = 0 :=
  i_exit_success { args := ["hello", "world"] }

end Lentils.True.Logic
