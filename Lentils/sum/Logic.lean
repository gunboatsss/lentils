/-
Sum.Logic — Pure BSD-style 16-bit checksum logic for `sum`. 0BSD

Computes the classic BSD `sum` checksum: a 16-bit rotating checksum over
the bytes, plus the block count (in 1KiB units, rounded up).
-/

namespace Lentils.sum.Logic

open ByteArray

/--
BSD 16-bit checksum. For each byte, rotate the running sum right by one bit
and add the byte (modulo 2^16).
-/
def bsdSum (data : ByteArray) : UInt16 :=
  data.foldl (λ (s : UInt16) (b : UInt8) =>
    let low := s.land 1
    let rotated := s.shiftRight 1 ||| (low.shiftLeft 15)
    let added := rotated + UInt16.ofNat b.toNat
    added.land 0xFFFF
  ) 0

/-- Block count in 1KiB units, rounded up. -/
def blockCount (data : ByteArray) : Nat :=
  (data.size + 1023) / 1024

def format (data : ByteArray) : String :=
  s!"{toString (bsdSum data)} {toString (blockCount data)}"

-- ─── Proofs ──────────────────────────────────────────────────────────────────

example : bsdSum ByteArray.empty = 0 := by native_decide
example : blockCount ByteArray.empty = 0 := by native_decide
example : bsdSum (ByteArray.mk #[0x61]) = 97 := by native_decide
example : format ByteArray.empty = "0 0" := by native_decide

end Lentils.sum.Logic
