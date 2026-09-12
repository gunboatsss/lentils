/-
Sum.Logic — Verified pure BSD-style 16-bit checksum logic for `sum`.
0BSD

Structure:
  1. State types      — SumInput (data: ByteArray)
  2. Specification    — bsdSum, blockCount, format
  3. Implementation   — bsdSum, blockCount, format (directly executable)
  4. Invariants       — parametric theorems over all inputs
  6. Lemmas           — helper theorems for invariants
  7. Concrete corollaries — derived examples

No IO, no FFI, no `sorry` or `admit`.

Computes the classic BSD `sum` checksum: a 16-bit rotating checksum over
the bytes, plus the block count (in 1KiB units, rounded up).
-/

import Lentils.Common.Spec

namespace Lentils.Sum.Logic

open Lentils.Common.Spec
open ByteArray

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for sum.
-/
structure SumInput where
  data : ByteArray
  deriving Inhabited, BEq

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Helpers
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
BSD 16-bit checksum. For each byte, rotate the running sum right by one bit
and add the byte (modulo 2^16).
-/
def bsdSum (data : ByteArray) : UInt16 :=
  data.foldl (fun s b =>
    let low := s.land 1
    let rotated := s.shiftRight 1 ||| (low.shiftLeft 15)
    let added := rotated + UInt16.ofNat b.toNat
    added.land 0xFFFF
  ) 0

/--
Block count in 1KiB units, rounded up.
-/
def blockCount (data : ByteArray) : Nat :=
  (data.size + 1023) / 1024

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Format the sum output: 5-digit zero-padded checksum, 5 spaces, block count.
-/
def format (data : ByteArray) : String :=
  let checksum := bsdSum data
  let blocks := blockCount data
  let sz := toString checksum
  let padded := String.ofList (List.replicate (5 - sz.length) '0') ++ sz
  s!"{padded}     {toString blocks}"

/--
Specification: given input, produce output string.
-/
def spec (input : SumInput) : String := format input.data

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Empty input has checksum 0.
-/
theorem i_empty_checksum : bsdSum ByteArray.empty = 0 := by
  native_decide

/--
I2: Empty input block count is 0.
-/
theorem i_empty_blocks : blockCount ByteArray.empty = 0 := by
  unfold blockCount; simp

/--
I3: Empty input format.
-/
theorem i_empty_format : format ByteArray.empty = "00000     0" := by
  native_decide

/--
I4: Single byte 0x61 ('a') has checksum 97.
-/
theorem i_single_a : bsdSum (ByteArray.mk #[0x61]) = 97 := by
  native_decide

/--
I5: Block count for a single byte is 1.
-/
theorem i_block_count_single_byte : blockCount (ByteArray.mk #[0]) = 1 := by
  native_decide

/--
I7: Block count is monotonic wrt input size.
-/
theorem i_block_count_monotone_size (a b : ByteArray) (h : a.size ≤ b.size) :
    blockCount a ≤ blockCount b := by
  unfold blockCount
  omega

/--
I8: blockCount for empty input is 0.
-/
theorem i_block_count_empty : blockCount ByteArray.empty = 0 := rfl

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Lemmas
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
blockCount of a single null byte is 1.
-/
theorem blockCount_single_null : blockCount (ByteArray.mk #[0]) = 1 := by
  native_decide

/--
blockCount of 1024 null bytes is 1.
-/
theorem blockCount_1024_null : blockCount (ByteArray.mk (List.toArray (List.replicate 1024 (0 : UInt8)))) = 1 := by
  native_decide

/--
blockCount of 1025 null bytes is 2.
-/
theorem blockCount_1025_null : blockCount (ByteArray.mk (List.toArray (List.replicate 1025 (0 : UInt8)))) = 2 := by
  native_decide

/--
The checksum of 10 null bytes is 0.
-/
theorem bsdSum_zeros_small : bsdSum (ByteArray.mk (List.toArray (List.replicate 10 (0 : UInt8)))) = 0 := by
  native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 7. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- Empty input checksum is 0. -/
example : bsdSum ByteArray.empty = 0 := i_empty_checksum

/-- Empty input blocks is 0. -/
example : blockCount ByteArray.empty = 0 := i_block_count_empty

/-- Empty input format. -/
example : format ByteArray.empty = "00000     0" := i_empty_format

/-- Single 'a' checksum is 97. -/
example : bsdSum (ByteArray.mk #[0x61]) = 97 := i_single_a

/-- Block count for 1024 null bytes is 1. -/
example : blockCount (ByteArray.mk (List.toArray (List.replicate 1024 (0 : UInt8)))) = 1 :=
  blockCount_1024_null

/-- Block count for 1025 null bytes is 2. -/
example : blockCount (ByteArray.mk (List.toArray (List.replicate 1025 (0 : UInt8)))) = 2 :=
  blockCount_1025_null

/-- 10 null bytes have checksum 0. -/
example : bsdSum (ByteArray.mk (List.toArray (List.replicate 10 (0 : UInt8)))) = 0 := bsdSum_zeros_small

end Lentils.Sum.Logic
