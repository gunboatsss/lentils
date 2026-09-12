/-
Arch.Logic — Verified pure logic for `arch`.
0BSD

Structure:
  1. State types      — ArchInput (no flags, no meaningful args)
  2. Specification    — arch string, format, spec (executable)
  3. Invariants       — parametric properties
  4. Concrete examples

No IO, no FFI, no `sorry` or `admit`.

POSIX: `arch` prints the machine architecture name.
-/

import Lentils.Common.Spec

namespace Lentils.Arch.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for arch. Arch ignores flags and arguments; the structure
exists for uniformity across all logic modules.
-/
structure ArchInput where
  args : List String
  deriving Inhabited, BEq

def defaultInput : ArchInput := { args := [] }

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
The machine architecture name. This is what `arch` prints.
On Linux this would come from /proc/sys/kernel/arch.
The spec defines it as a pure string; IO resolution is separate.
-/
def arch : String := "unknown"

/--
Validate an architecture string: it must be non-empty and not the
fallback "unknown".
-/
def isValid (s : String) : Bool :=
  s ≠ "" && s ≠ "unknown"

/--
Format the arch output for a given input. Since arch ignores all
arguments, this is just the arch string itself.
-/
def format (input : ArchInput) : String :=
  arch

/--
Specification: the pure function defining correct behavior.
-/
def spec (input : ArchInput) : String :=
  format input

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Parametric Invariants
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: The default input produces the architecture string.
-/
theorem i_default : format defaultInput = arch := rfl

/--
I2: The arch string is non-empty (follows from its value).
-/
theorem i_nonempty : arch ≠ "" := by
  unfold arch; decide

/--
I3: Arguments do not affect the output (arch ignores them).
-/
theorem i_args_irrelevant (args : List String) :
    format { args := args } = format { args := [] } := rfl

/--
I3b: Spec output is constant across all inputs (∀-quantified invariant).
-/
theorem i_spec_const (a b : ArchInput) : spec a = spec b := by
  simp [spec, format, arch]

/--
I5: An empty string is not a valid architecture name.
-/
theorem i_invalid_empty : isValid "" = false := by
  native_decide

/--
I6: The fallback "unknown" is not a valid architecture name.
-/
theorem i_invalid_unknown : isValid "unknown" = false := by
  native_decide

/--
I7: Well-known architectures are recognised as valid.
-/
theorem i_valid_x86_64 : isValid "x86_64" = true := by
  native_decide

theorem i_valid_aarch64 : isValid "aarch64" = true := by
  native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- arch default output is the arch string. -/
example : format defaultInput = "unknown" := rfl

/-- x86_64 is valid. -/
example : isValid "x86_64" = true := i_valid_x86_64

end Lentils.Arch.Logic
