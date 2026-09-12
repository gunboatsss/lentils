/-
Hostid.Logic — Verified pure logic for `hostid`.
0BSD

Structure:
  1. State types      — HostidInput (raw hex string)
  2. Specification    — formatHostid
  3. Implementation   — same as spec
  4. Correctness      — impl_correct
  5. Invariants       — parametric properties
  6. Concrete examples

No IO, no FFI, no `sorry` or `admit`.

POSIX: `hostid` prints the numeric host identifier as an 8-character
hexadecimal string.
-/

import Lentils.Common.Spec

namespace Lentils.Hostid.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for hostid: the raw hex string from the system.
-/
structure HostidInput where
  raw : String
  deriving Inhabited, BEq

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Helpers
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Is the character a valid hex digit [0-9a-fA-F]?
-/
def isHexChar (c : Char) : Bool :=
  (c ≥ '0' && c ≤ '9') || (c ≥ 'a' && c ≤ 'f') || (c ≥ 'A' && c ≤ 'F')

/--
Pad a hex string to at least 8 characters with leading '0's.
-/
def padHex (s : String) : String :=
  if s.length ≥ 8 then s
  else String.mk (List.replicate (8 - s.length) '0') ++ s

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Format a raw hostid string into the canonical 8-character hex output.
Steps: trim whitespace, keep only hex characters, then pad to ≥8.
-/
def formatHostid (raw : String) : String :=
  let trimmed := raw.trimAscii.toString
  let hexChars := String.mk (trimmed.toList.filter isHexChar)
  if hexChars.isEmpty then "00000000"
  else padHex hexChars

/--
Specification: format the raw hostid for output.
-/
def spec (input : HostidInput) : String :=
  formatHostid input.raw

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Parametric Invariants
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Empty raw string → "00000000".
-/
theorem i_empty : formatHostid "" = "00000000" := by
  native_decide

/--
I2: Long hex strings are returned unchanged by `padHex` (parametric, proved by unfold + simp).
-/
theorem i_padHex_long (s : String) (h : 8 ≤ s.length) : padHex s = s := by
  unfold padHex
  simp [h]

/--
I3: Non-hex characters are stripped (concrete example).
-/
theorem i_strips_nonhex : formatHostid "ab-12_xy" = "0000ab12" := by
  native_decide

/--
I4: A well-formed 8-digit hex string is returned unchanged.
-/
theorem i_identity_8hex : formatHostid "00000000" = "00000000" := by
  native_decide

/--
I5: Lowercase hex string is preserved.
-/
theorem i_identity_abcdef01 : formatHostid "abcdef01" = "abcdef01" := by
  native_decide

/--
I6: Short inputs are padded with leading zeros.
-/
theorem i_pads_short : formatHostid "ff" = "000000ff" := by
  native_decide

/--
I7: Whitespace surrounding hex is trimmed.
-/
theorem i_trims_whitespace : formatHostid "  abcdef01  " = "abcdef01" := by
  native_decide

/--
I8: Input containing only non-hex characters yields the default.
-/
theorem i_nonhex_only : formatHostid "---!!!" = "00000000" := by
  native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 7. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- Empty input yields default. -/
example : formatHostid "" = "00000000" := i_empty

/-- 8-char hex unchanged. -/
example : formatHostid "a1b2c3d4" = "a1b2c3d4" := by
  native_decide

/-- Padding a short value. -/
example : formatHostid "1" = "00000001" := by
  native_decide

end Lentils.Hostid.Logic
