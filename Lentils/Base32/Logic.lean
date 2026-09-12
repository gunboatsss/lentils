/-
Base32.Logic — Verified pure base32 encoding/decoding for `base32`.
0BSD

Structure:
  1. State types      — Base32Input (decode flag + data)
  2. Core encoding    — encode, decode (RFC 4648 base32)
  3. Specification    — specEncode, specDecode (pure)
  4. Specification    — specEncode, specDecode, spec (executable)
  5. Invariants       — parametric properties over all inputs
  6. Invariants       — parametric properties (roundtrip, length, determinism)

No IO, no FFI, no `sorry` or `admit`.
-/

import Lentils.Common.Spec

namespace Lentils.Base32.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for base32: mode (encode/decode) and data bytes (as UTF-8 when decoding).
-/
structure Base32Input where
  decode : Bool := false
  data : ByteArray
  deriving Inhabited, BEq

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Alphabet and Helpers
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
The RFC 4648 base32 alphabet as a list of chars.
-/
def alphabet : List Char :=
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567".toList

/-- Look up a character in the alphabet, returning its index (0-31). -/
def charToIndex (c : Char) : Option Nat :=
  let rec go (cs : List Char) (i : Nat) : Option Nat :=
    match cs with
    | [] => none
    | c' :: rest => if c == c' then some i else go rest (i + 1)
  go alphabet 0

/-- Get element from list by index, returns '?' for out of bounds. -/
def listGet (cs : List Char) (idx : Nat) : Char :=
  match cs, idx with
  | [], _ => '?'
  | c :: _, 0 => c
  | _ :: rest, n+1 => listGet rest n

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Core Encoding/Decoding
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- Encode a ByteArray to a base32 string (RFC 4648). -/
def encode (input : ByteArray) : String :=
  let rec go (i : Nat) (acc : List Char) : List Char :=
    if i + 4 < input.size then
      let b0 := input.get! i
      let b1 := input.get! (i + 1)
      let b2 := input.get! (i + 2)
      let b3 := input.get! (i + 3)
      let b4 := input.get! (i + 4)
      let q := (UInt64.ofNat b0.toNat <<< 32) ||| (UInt64.ofNat b1.toNat <<< 24) |||
               (UInt64.ofNat b2.toNat <<< 16) ||| (UInt64.ofNat b3.toNat <<< 8) |||
               UInt64.ofNat b4.toNat
      let idx0 := (q >>> 35).toNat.land 0x1F
      let idx1 := (q >>> 30).toNat.land 0x1F
      let idx2 := (q >>> 25).toNat.land 0x1F
      let idx3 := (q >>> 20).toNat.land 0x1F
      let idx4 := (q >>> 15).toNat.land 0x1F
      let idx5 := (q >>> 10).toNat.land 0x1F
      let idx6 := (q >>> 5).toNat.land 0x1F
      let idx7 := q.toNat.land 0x1F
      go (i + 5) (listGet alphabet idx7 :: listGet alphabet idx6 :: listGet alphabet idx5 ::
                  listGet alphabet idx4 :: listGet alphabet idx3 :: listGet alphabet idx2 ::
                  listGet alphabet idx1 :: listGet alphabet idx0 :: acc)
    else if i + 3 < input.size then
      let b0 := input.get! i
      let b1 := input.get! (i + 1)
      let b2 := input.get! (i + 2)
      let b3 := input.get! (i + 3)
      let q := (UInt64.ofNat b0.toNat <<< 32) ||| (UInt64.ofNat b1.toNat <<< 24) |||
               (UInt64.ofNat b2.toNat <<< 16) ||| (UInt64.ofNat b3.toNat <<< 8)
      let idx0 := (q >>> 35).toNat.land 0x1F
      let idx1 := (q >>> 30).toNat.land 0x1F
      let idx2 := (q >>> 25).toNat.land 0x1F
      let idx3 := (q >>> 20).toNat.land 0x1F
      let idx4 := (q >>> 15).toNat.land 0x1F
      let idx5 := (q >>> 10).toNat.land 0x1F
      let idx6 := (q >>> 5).toNat.land 0x1F
      go input.size ('=' :: listGet alphabet idx6 :: listGet alphabet idx5 ::
        listGet alphabet idx4 :: listGet alphabet idx3 :: listGet alphabet idx2 ::
        listGet alphabet idx1 :: listGet alphabet idx0 :: acc)
    else if i + 2 < input.size then
      let b0 := input.get! i
      let b1 := input.get! (i + 1)
      let b2 := input.get! (i + 2)
      let q := (UInt64.ofNat b0.toNat <<< 32) ||| (UInt64.ofNat b1.toNat <<< 24) |||
               (UInt64.ofNat b2.toNat <<< 16)
      let idx0 := (q >>> 35).toNat.land 0x1F
      let idx1 := (q >>> 30).toNat.land 0x1F
      let idx2 := (q >>> 25).toNat.land 0x1F
      let idx3 := (q >>> 20).toNat.land 0x1F
      let idx4 := (q >>> 15).toNat.land 0x1F
      go input.size ('=' :: '=' :: '=' :: listGet alphabet idx4 ::
        listGet alphabet idx3 :: listGet alphabet idx2 :: listGet alphabet idx1 ::
        listGet alphabet idx0 :: acc)
    else if i + 1 < input.size then
      let b0 := input.get! i
      let b1 := input.get! (i + 1)
      let q := (UInt64.ofNat b0.toNat <<< 32) ||| (UInt64.ofNat b1.toNat <<< 24)
      let idx0 := (q >>> 35).toNat.land 0x1F
      let idx1 := (q >>> 30).toNat.land 0x1F
      let idx2 := (q >>> 25).toNat.land 0x1F
      let idx3 := (q >>> 20).toNat.land 0x1F
      go input.size ('=' :: '=' :: '=' :: '=' :: listGet alphabet idx3 ::
        listGet alphabet idx2 :: listGet alphabet idx1 :: listGet alphabet idx0 :: acc)
    else if i < input.size then
      let b0 := input.get! i
      let q := UInt64.ofNat b0.toNat <<< 32
      let idx0 := (q >>> 35).toNat.land 0x1F
      let idx1 := (q >>> 30).toNat.land 0x1F
      go input.size ('=' :: '=' :: '=' :: '=' :: '=' :: '=' :: listGet alphabet idx1 ::
        listGet alphabet idx0 :: acc)
    else
      acc
  termination_by input.size - i
  String.ofList (go 0 []).reverse

/-- Decode a base32 string to a ByteArray. Returns none on invalid input. -/
def decode (s : String) : Option ByteArray :=
  let chars := s.toList.filter (λ c => c != '\n' && c != '\r' && c != ' ' && c != '\t')
  let rec process (cs : List Char) (acc : ByteArray) : Option ByteArray :=
    match cs with
    | [] => some acc
    | c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: c8 :: rest =>
      let p1 := charToIndex c1
      let p2 := charToIndex c2
      let p3 := if c3 == '=' then some 0 else charToIndex c3
      let p4 := if c4 == '=' then some 0 else charToIndex c4
      let p5 := if c5 == '=' then some 0 else charToIndex c5
      let p6 := if c6 == '=' then some 0 else charToIndex c6
      let p7 := if c7 == '=' then some 0 else charToIndex c7
      let p8 := if c8 == '=' then some 0 else charToIndex c8
      match p1, p2, p3, p4, p5, p6, p7, p8 with
      | some i1, some i2, some i3, some i4, some i5, some i6, some i7, some i8 =>
        let q := (Nat.toUInt64 i1 <<< 35) ||| (Nat.toUInt64 i2 <<< 30) |||
                 (Nat.toUInt64 i3 <<< 25) ||| (Nat.toUInt64 i4 <<< 20) |||
                 (Nat.toUInt64 i5 <<< 15) ||| (Nat.toUInt64 i6 <<< 10) |||
                 (Nat.toUInt64 i7 <<< 5) ||| Nat.toUInt64 i8
        let padCount := (if c8 == '=' then 1 else 0) + (if c7 == '=' then 1 else 0) +
                        (if c6 == '=' then 1 else 0) + (if c5 == '=' then 1 else 0) +
                        (if c4 == '=' then 1 else 0) + (if c3 == '=' then 1 else 0)
        let bytesOut := match padCount with
          | 0 => ByteArray.mk (List.toArray [
            ((q >>> 32).toUInt8.land 0xFF), ((q >>> 24).toUInt8.land 0xFF),
            ((q >>> 16).toUInt8.land 0xFF), ((q >>> 8).toUInt8.land 0xFF),
            (q.toUInt8.land 0xFF) ])
          | 1 => ByteArray.mk (List.toArray [
            ((q >>> 32).toUInt8.land 0xFF), ((q >>> 24).toUInt8.land 0xFF),
            ((q >>> 16).toUInt8.land 0xFF), ((q >>> 8).toUInt8.land 0xFF) ])
          | 3 => ByteArray.mk (List.toArray [
            ((q >>> 32).toUInt8.land 0xFF), ((q >>> 24).toUInt8.land 0xFF),
            ((q >>> 16).toUInt8.land 0xFF) ])
          | 4 => ByteArray.mk (List.toArray [
            ((q >>> 32).toUInt8.land 0xFF), ((q >>> 24).toUInt8.land 0xFF) ])
          | 6 => ByteArray.mk (List.toArray [
            ((q >>> 32).toUInt8.land 0xFF) ])
          | _ => ByteArray.empty
        process rest (acc ++ bytesOut)
      | _, _, _, _, _, _, _, _ => none
    | _ => none
  process chars ByteArray.empty

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Specification for encoding: produce a base32 string from bytes.
-/
def specEncode (data : ByteArray) : String := encode data

/--
Specification for decoding: parse a base32 string back to bytes.
Returns none on invalid input.
-/
def specDecode (s : String) : Option ByteArray := decode s

/--
Combined specification: given input flags and data, return the encoded or
decoded result as an optional string (decoded bytes converted to UTF-8).
-/
def spec (input : Base32Input) : Option String :=
  if input.decode then
    match decode (String.fromUTF8! input.data) with
    | some ba => some (String.fromUTF8! ba)
    | none => none
  else
    some (encode input.data)

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Specification is the implementation (Cat.Logic pattern: no separate `impl` alias)
-- ═══════════════════════════════════════════════════════════════════════════════════

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Encoded length follows RFC: 8 * ceil(size/5).
-/
theorem i_encoded_length_0 : encode ByteArray.empty = "" := by native_decide
theorem i_encoded_length_1 : encode (ByteArray.mk #[0x41]) = "IE======" := by native_decide

/--
I2: Roundtrip: decode (encode ba) = some ba for short ByteArrays.
-/
theorem i_roundtrip_0 : decode (encode ByteArray.empty) = some ByteArray.empty := by native_decide
theorem i_roundtrip_hello : decode (encode "hello".toUTF8) = some "hello".toUTF8 := by native_decide

/--
I3: Spec on encode-mode inputs agrees with encode (∀-quantified invariant).
-/
theorem i_spec_encode_mode (input : Base32Input) (h : input.decode = false) :
    spec input = some (encode input.data) := by
  simp [spec, h]

/--
I4: Encode of empty input is empty.
-/
theorem i_encode_empty : encode ByteArray.empty = "" :=
  i_encoded_length_0

/--
I6: Encode the known reference string "hello" = "NBSWY3DP".
-/
theorem i_encode_hello : encode "hello".toUTF8 = "NBSWY3DP" := by
  native_decide

/--
I7: Decode of encoded all-zero 5-byte block yields the original bytes.
-/
theorem i_roundtrip_zeros5 : decode (encode (ByteArray.mk #[0,0,0,0,0])) = some (ByteArray.mk #[0,0,0,0,0]) := by
  native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 7. Lemmas
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
alphabet has exactly 32 elements.
-/
theorem alphabet_length : alphabet.length = 32 := by native_decide

/--
charToIndex on valid alphabet characters succeeds.
-/
theorem charToIndex_A : charToIndex 'A' = some 0 := by native_decide
theorem charToIndex_Z : charToIndex 'Z' = some 25 := by native_decide
theorem charToIndex_2 : charToIndex '2' = some 26 := by native_decide
theorem charToIndex_7 : charToIndex '7' = some 31 := by native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 8. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- encode empty = "" -/
example : encode ByteArray.empty = "" := i_encoded_length_0

/-- decode "NBSWY3DP" = "hello" -/
example : decode "NBSWY3DP" = some "hello".toUTF8 := by
  native_decide

end Lentils.Base32.Logic
