/-
Sum.Logic — Pure BSD-style 16-bit checksum logic for `sum`. 0BSD

Computes the classic BSD `sum` checksum: a 16-bit rotating checksum over
the bytes, plus the block count (in 1KiB units, rounded up).
-/

namespace Lentils.Sum.Logic

open ByteArray

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

/-- Block count in 1KiB units, rounded up. -/
def blockCount (data : ByteArray) : Nat :=
  (data.size + 1023) / 1024

def format (data : ByteArray) : String :=
  let checksum := bsdSum data
  let blocks := blockCount data
  -- BSD sum format: 5-digit zero-padded checksum + 5 spaces + block count
  let sz := toString checksum
  let padded := String.ofList (List.replicate (5 - sz.length) '0') ++ sz
  s!"{padded}     {toString blocks}"

-- ─── Proofs ──────────────────────────────────────────────────────────────────

-- Main sum test vectors
example : bsdSum ByteArray.empty = 0 := by native_decide
example : blockCount ByteArray.empty = 0 := by native_decide
example : bsdSum (ByteArray.mk #[0x61]) = 97 := by native_decide
example : format ByteArray.empty = "00000     0" := by native_decide

-- ─── Intermediate Function Proofs ─────────────────────────────────────────────

-- Block count edge cases
example : blockCount (ByteArray.mk (List.toArray (List.range 1024 |>.map fun _ => (0x41 : UInt8)))) = 1 := by native_decide
example : blockCount (ByteArray.mk (List.toArray (List.range 1025 |>.map fun _ => (0x41 : UInt8)))) = 2 := by native_decide

-- Sum rotation properties
example : (0 : UInt16).land 1 = 0 := rfl
example : (0xFFFF : UInt16).land 1 = 1 := by native_decide

end Lentils.Sum.Logic