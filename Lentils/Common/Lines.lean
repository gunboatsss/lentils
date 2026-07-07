/-
Common.Lines — Verified line split/join primitives for lean-coreutils.
0BSD

This file contains ONLY pure functions — no IO, no FFI.
Formal proofs are at the bottom.
No `sorry` or `admit` allowed.

Provides line-oriented operations built on Bytes.splitOnNewline and joinWithNewline.

Provenance: implemented from POSIX line-termination semantics (0x0A = '\n').
No GPL source was consulted.
-/

import Lentils.Common.Bytes

namespace Lentils.Common.Lines

open ByteArray
open Lentils.Common.Bytes

/-- Split a ByteArray into lines (ByteArrays without the newline separator).
    The final element may be empty if the input ends with a newline.
    This follows POSIX line semantics. -/
def splitLines (ba : ByteArray) : List ByteArray :=
  splitOnNewline ba

/-- Join lines with newline separators, producing a ByteArray. -/
def joinLines (lines : List ByteArray) : ByteArray :=
  joinWithNewline lines

/-- Count the number of newline characters (i.e., number of line splits). -/
def countLines (ba : ByteArray) : Nat :=
  countNewlines ba

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- Splitting then joining an empty ByteArray returns empty. -/
example : joinLines (splitLines ByteArray.empty) = ByteArray.empty := by
  native_decide

/-- Splitting a single line without a trailing newline: roundtrip holds. -/
example : joinLines (splitLines (ByteArray.mk #[0x41])) = ByteArray.mk #[0x41] := by
  native_decide

/-- A single empty line (just a newline): splitting yields two empty ByteArrays. -/
example : splitLines (ByteArray.mk #[0x0A]) = [ByteArray.empty, ByteArray.empty] := by
  native_decide

/-- Joining two lines yields a newline-separated ByteArray. -/
example (a b : ByteArray) : joinLines [a, b] = a.push 0x0A ++ b := rfl

/-- Splitting a ByteArray without newlines yields a singleton. -/
example : splitLines (ByteArray.mk #[0x41, 0x42]) = [ByteArray.mk #[0x41, 0x42]] := by
  native_decide

/-- countLines of empty ByteArray is zero. -/
example : countLines ByteArray.empty = 0 := rfl

/-- countLines counts newline bytes. -/
example : countLines (ByteArray.mk #[0x0A, 0x42, 0x0A]) = 2 := rfl

/-- A ByteArray with no newlines has 0 lines counted. -/
example : countLines (ByteArray.mk #[0x41, 0x42, 0x43]) = 0 := rfl

end Lentils.Common.Lines
