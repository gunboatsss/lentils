/-
Cksum.Logic — Pure POSIX `cksum` CRC-32 logic. 0BSD

Implements the POSIX cksum algorithm: a CRC-32 (polynomial 0xEDB88320,
reflected) computed over the data, then the 32-bit length folded in as
four big-endian bytes, finally inverted.
-/

namespace Lentils.Cksum.Logic

open ByteArray

/-- Total list indexing with a fallback (List.get! is unavailable). -/
def listGet {α} (l : List α) (i : Nat) (d : α) : α :=
  let rec go (xs : List α) (j : Nat) : α :=
    match xs with
    | [] => d
    | x :: xs => if j = 0 then x else go xs (j - 1)
  go l i

/-- Build the 256-entry reflected CRC table. -/
def buildCrcTable : List UInt32 :=
  let rec entry (c : UInt32) (k : Nat) : UInt32 :=
    if k = 0 then c
    else
      let c' := if (c.land 1) = 1 then UInt32.xor (c.shiftRight 1) (0xEDB88320 : UInt32) else c.shiftRight 1
      entry c' (k - 1)
  termination_by k
  let rec fill (i : Nat) (acc : List UInt32) : List UInt32 :=
    if i = 0 then acc else fill (i - 1) (entry (UInt32.ofNat (i - 1)) 8 :: acc)
  termination_by i
  fill 256 []

/-- One CRC step using a precomputed table. -/
def crcStep (table : List UInt32) (crc : UInt32) (b : UInt8) : UInt32 :=
  let idx := (UInt32.xor (crc.shiftRight 24) (UInt32.ofNat b.toNat)).land 0xFF
  UInt32.xor (crc.shiftLeft 8) (listGet table idx.toNat 0)

/-- Fold a single byte (most-significant first) into the running CRC. -/
def crcStepByte (table : List UInt32) (crc : UInt32) (b : UInt32) : UInt32 :=
  let idx := (UInt32.xor (crc.shiftRight 24) b).land 0xFF
  UInt32.xor (crc.shiftLeft 8) (listGet table idx.toNat 0)

/--
Compute the POSIX cksum: returns (crc, byteCount).
-/
def cksum (data : ByteArray) : UInt32 × Nat :=
  let table := buildCrcTable
  let crc0 := data.foldl (crcStep table) (0 : UInt32)
  let len := data.size
  let len32 := UInt32.ofNat len
  let (crc1, _) :=
    (List.range 4).foldl (λ (acc : UInt32 × UInt32) (_ : Nat) =>
      let (crc, l32) := acc
      let byte := (l32.shiftRight 24).land 0xFF
      (crcStepByte table crc byte, l32.shiftLeft 8)
    ) (crc0, len32)
  (crc1, len)

def format (data : ByteArray) : String :=
  let (crc, len) := cksum data
  s!"{toString crc} {toString len}"

-- ─── Proofs ──────────────────────────────────────────────────────────────────

example : cksum ByteArray.empty = (0, 0) := by native_decide
example : format ByteArray.empty = "0 0" := by native_decide

end Lentils.Cksum.Logic
