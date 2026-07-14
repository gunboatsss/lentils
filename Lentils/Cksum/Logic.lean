/-
Cksum.Logic — Pure POSIX `cksum` CRC-32 logic. 0BSD

Implements the POSIX cksum algorithm per IEEE Std 1003.1-2024:
CRC-32 based on ISO/IEC 8802-3:1996 (Ethernet) with length folding.
-/

namespace Lentils.Cksum.Logic

open ByteArray

/-- Safe array indexing with fallback for CRC table. -/
def arrGet (arr : Array UInt32) (i : Nat) : UInt32 :=
  if h : i < arr.size then arr[i] else 0

/-- Build the 256-entry CRC table per POSIX (MSB-first, polynomial 0x04C11DB7). -/
def buildCrcTable : Array UInt32 := Id.run do
  let mut table : Array UInt32 := #[]
  for i in [0:256] do
    let mut crc : UInt32 := UInt32.ofNat i
    crc := crc.shiftLeft 24
    for _ in [0:8] do
      if (crc.land 0x80000000) = 0x80000000 then
        crc := UInt32.xor (crc.shiftLeft 1) (0x04C11DB7 : UInt32)
      else
        crc := crc.shiftLeft 1
    table := table.push crc
  return table

/-- One CRC step using a precomputed table. -/
def crcStep (table : Array UInt32) (crc : UInt32) (b : UInt8) : UInt32 :=
  let idx := (UInt32.xor (crc.shiftRight 24) (UInt32.ofNat b.toNat)).land 0xFF
  UInt32.xor (crc.shiftLeft 8) (arrGet table idx.toNat)

/-- Fold a single byte into the running CRC. -/
def crcStepByte (table : Array UInt32) (crc : UInt32) (b : UInt32) : UInt32 :=
  let idx := (UInt32.xor (crc.shiftRight 24) b).land 0xFF
  UInt32.xor (crc.shiftLeft 8) (arrGet table idx.toNat)

/--
Compute the POSIX cksum per IEEE 1003.1-2024:
Initial value is 0. Fold in length using low-order bytes until zero.
Final result is complemented (XOR with 0xFFFFFFFF).
-/
def cksum (data : ByteArray) : UInt32 × Nat :=
  let table := buildCrcTable
  let crc0 := data.foldl (crcStep table) (0 : UInt32)
  let len := data.size
  -- Fold in length in little-endian order using while loop (stop when len == 0)
  let rec foldLen (crc : UInt32) (l : Nat) : UInt32 :=
    if l = 0 then crc
    else
      let byte := UInt32.ofNat (l % 256)
      foldLen (crcStepByte table crc byte) (l / 256)
  let crc1 := foldLen crc0 len
  (UInt32.xor crc1 (0xFFFFFFFF : UInt32), len)

def format (data : ByteArray) : String :=
  let (crc, len) := cksum data
  s!"{toString crc} {toString len}"

-- ─── Proofs ──────────────────────────────────────────────────────────────────

-- Main cksum test vectors (POSIX spec)
example : cksum ByteArray.empty = (4294967295, 0) := by native_decide
example : format ByteArray.empty = "4294967295 0" := by native_decide

-- ─── Intermediate Function Proofs ─────────────────────────────────────────────

-- CRC table properties
example : buildCrcTable.size = 256 := by native_decide

-- Block count
example : (ByteArray.empty).size = 0 := rfl

end Lentils.Cksum.Logic