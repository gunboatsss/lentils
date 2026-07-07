-- Cat.Logic — Pure byte processing for `cat`.
--
-- This file contains ONLY pure functions — no IO, no FFI.
-- Formal proofs are at the bottom.
-- No `sorry` or `admit` allowed.

namespace Lentils.Cat.Logic

open ByteArray

/-- processBytes is the identity function.
    `cat` passes input bytes straight to output without modification.
    This is the simplest possible verified core. -/
def processBytes (ba : ByteArray) : ByteArray := ba

/-- Concatenate multiple ByteArrays into one, in order. -/
def concatenateByteArrays (bas : List ByteArray) : ByteArray :=
  bas.foldl (· ++ ·) ByteArray.empty

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- `processBytes` is the identity function.
    This is the formal specification for `cat`: output equals input. -/
theorem processBytes_id (ba : ByteArray) : processBytes ba = ba := rfl

/-- `processBytes` composed with itself is `processBytes` (idempotence).
    This follows trivially from the identity property. -/
theorem processBytes_idempotent (ba : ByteArray) : processBytes (processBytes ba) = processBytes ba := by
  simp [processBytes]

/-- Concatenating a singleton list is the same as its element. -/
example (ba : ByteArray) : concatenateByteArrays [ba] = ba := by
  simp [concatenateByteArrays]

/-- Concatenating the empty list yields the empty ByteArray. -/
example : concatenateByteArrays ([] : List ByteArray) = ByteArray.empty := by
  simp [concatenateByteArrays]

end Lentils.Cat.Logic
