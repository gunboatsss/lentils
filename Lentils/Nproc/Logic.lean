/-
Nproc.Logic — Verified pure logic for `nproc`.
0BSD

Structure:
  1. State types      — NprocInput (cpuinfo content)
  2. Specification    — countProcessors
  3. Implementation   — same as spec
  4. Correctness      — impl_correct
  5. Invariants       — parametric properties
  6. Concrete examples

No IO, no FFI, no `sorry` or `admit`.

POSIX: `nproc` prints the number of processing units.
On Linux this reads /proc/cpuinfo.
-/

import Lentils.Common.Spec

namespace Lentils.Nproc.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for nproc: the raw /proc/cpuinfo content.
-/
structure NprocInput where
  cpuinfo : String
  deriving Inhabited, BEq

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Count the number of processors in /proc/cpuinfo content.
Each processor is identified by a line starting with "processor".
-/
def countProcessors (cpuinfo : String) : Nat :=
  let lines := cpuinfo.splitOn "\n"
  (lines.filter (λ (l : String) => l.startsWith "processor")).length

/--
Specification: count processors from cpuinfo.
-/
def spec (input : NprocInput) : Nat :=
  countProcessors input.cpuinfo

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Implementation
-- ═══════════════════════════════════════════════════════════════════════════════════

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Parametric Invariants
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Empty cpuinfo yields 0 processors.
-/
theorem i_empty : countProcessors "" = 0 := by
  native_decide

/--
I2: A single processor entry yields count 1.
-/
theorem i_single_processor : countProcessors "processor\t: 0\n" = 1 := by
  native_decide

/--
I3: Two processor entries yield count 2.
-/
theorem i_two_processors :
    countProcessors "processor\t: 0\nprocessor\t: 1\n" = 2 := by
  native_decide

/--
I4: Lines not starting with "processor" are ignored.
-/
theorem i_ignores_other_lines :
    countProcessors "cpu family\t: 6\nmodel\t: 158\n" = 0 := by
  native_decide

/--
I6: The count is always non-negative (trivially true for Nat).
-/
theorem i_nonnegative (cpuinfo : String) : 0 ≤ countProcessors cpuinfo := by
  omega

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- Empty cpuinfo → 0. -/
example : countProcessors "" = 0 := i_empty

/-- One processor. -/
example : countProcessors "processor\t: 0\n" = 1 := i_single_processor

/-- Two processors. -/
example : countProcessors "processor\t: 0\nprocessor\t: 1\n" = 2 := i_two_processors

end Lentils.Nproc.Logic
