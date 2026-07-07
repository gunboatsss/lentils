/-
Common.Bytes — Verified byte-array primitives for lean-coreutils.
0BSD

This file contains ONLY pure functions — no IO, no FFI.
Formal proofs are at the bottom.
No `sorry` or `admit` allowed.

Provenance: implemented from POSIX/ByteArray API.
No GPL source was consulted.
-/

namespace Lentils.Common.Bytes

open ByteArray

/-- Count occurrences of a byte in a ByteArray. -/
def countByte (ba : ByteArray) (b : UInt8) : Nat :=
  ba.foldl (λ acc byte => if byte == b then acc + 1 else acc) 0

/-- Split a ByteArray on a delimiter byte.
    Returns a list of ByteArrays; the delimiter byte is excluded.
    Empty elements are included (e.g., splitting "a\n\nb" on '\n' yields ["a", "", "b"]).
    Uses structural recursion on the index to avoid `partial`. -/
def splitOn (ba : ByteArray) (delim : UInt8) : List ByteArray :=
  let rec go (i : Nat) (current : ByteArray) : List ByteArray :=
    if i < ba.size then
      let b := ba.get! i
      if b == delim then
        current :: go (i + 1) ByteArray.empty
      else
        go (i + 1) (current.push b)
    else
      [current]
  go 0 ByteArray.empty

/-- Split on newline byte (0x0A). -/
def splitOnNewline (ba : ByteArray) : List ByteArray :=
  splitOn ba 0x0A

/-- Count newline bytes (i.e., number of lines minus one for a non-empty file
    that ends with a newline; plus one if the file doesn't end with newline). -/
def countNewlines (ba : ByteArray) : Nat :=
  countByte ba 0x0A

/-- Join a list of ByteArrays with a separator byte. -/
def joinWith (bas : List ByteArray) (sep : UInt8) : ByteArray :=
  match bas with
  | [] => ByteArray.empty
  | [x] => x
  | x :: xs =>
    xs.foldl (λ acc ba => acc.push sep ++ ba) x

/-- Join with newline byte. -/
def joinWithNewline (bas : List ByteArray) : ByteArray :=
  joinWith bas 0x0A

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- Count of a byte is zero in the empty ByteArray. -/
theorem countByte_empty (b : UInt8) : countByte ByteArray.empty b = 0 := rfl

/-- Count of a byte equals the number of occurrences.
    This is a simple sanity check. -/
example : countByte (ByteArray.mk #[0x0A, 0x42, 0x0A]) 0x0A = 2 := rfl

/-- Joining an empty list yields the empty ByteArray. -/
theorem joinWithNewline_empty : joinWithNewline ([] : List ByteArray) = ByteArray.empty := rfl

/-- Joining a singleton list yields the element unchanged. -/
example (ba : ByteArray) : joinWithNewline [ba] = ba := rfl

/-- Joining two byte arrays with newline yields the expected concatenation. -/
example (a b : ByteArray) : joinWithNewline [a, b] = a.push 0x0A ++ b := rfl

/-- Splitting the empty ByteArray yields a singleton list of empty. -/
example : splitOnNewline (ByteArray.empty) = [ByteArray.empty] := by
  native_decide

/-- Splitting a byte with no newlines yields a singleton. -/
example : splitOnNewline (ByteArray.mk #[0x42, 0x43]) = [ByteArray.mk #[0x42, 0x43]] := by
  native_decide

/-- Splitting on a single newline yields two empty ByteArrays. -/
example : splitOnNewline (ByteArray.mk #[0x0A]) = [ByteArray.empty, ByteArray.empty] := by
  native_decide

/-- Splitting "AB\nCD" yields ["AB", "CD"]. -/
example : splitOnNewline (ByteArray.mk #[0x41, 0x42, 0x0A, 0x43, 0x44]) = [ByteArray.mk #[0x41, 0x42], ByteArray.mk #[0x43, 0x44]] := by
  native_decide

end Lentils.Common.Bytes
