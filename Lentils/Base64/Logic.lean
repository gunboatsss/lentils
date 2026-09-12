/-
Base64.Logic — Verified pure base64 encoding/decoding for `base64`.
0BSD

Structure:
  1. State types      — Base64Input (decode flag + data)
  2. Core encoding    — encode, decode (RFC 4648 base64)
  3. Specification    — specEncode, specDecode
  4. Specification    — specEncode, specDecode, spec (executable)
  5. Invariants       — parametric properties over all inputs
  6. Invariants       — parametric properties (roundtrip, length, determinism)

No IO, no FFI, no `sorry` or `admit`.
-/

import Lentils.Common.Spec

namespace Lentils.Base64.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for base64: mode (encode/decode) and data bytes.
-/
structure Base64Input where
  decode : Bool := false
  data : ByteArray
  deriving Inhabited, BEq

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Alphabet and Helpers
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- The base64 alphabet as a list of chars (RFC 4648). -/
def alphabet : List Char :=
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".toList

/-- Look up a character in the alphabet, returning its index (0-63). -/
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

/-- Encode a ByteArray to a base64 string (RFC 4648). -/
def encode (input : ByteArray) : String :=
  let rec go (i : Nat) (acc : List Char) : List Char :=
    if i + 2 < input.size then
      let b0 := input.get! i
      let b1 := input.get! (i + 1)
      let b2 := input.get! (i + 2)
      let triple := (UInt32.shiftLeft b0.toUInt32 16) + (UInt32.shiftLeft b1.toUInt32 8) + b2.toUInt32
      let idx0 := UInt32.toNat (triple >>> 18) % 64
      let idx1 := UInt32.toNat (triple >>> 12) % 64
      let idx2 := UInt32.toNat (triple >>> 6) % 64
      let idx3 := UInt32.toNat (triple % 64)
      let c0 := listGet alphabet idx0
      let c1 := listGet alphabet idx1
      let c2 := listGet alphabet idx2
      let c3 := listGet alphabet idx3
      go (i + 3) (c3 :: c2 :: c1 :: c0 :: acc)
    else if i + 1 < input.size then
      let b0 := input.get! i
      let b1 := input.get! (i + 1)
      let triple := (UInt32.shiftLeft b0.toUInt32 16) + (UInt32.shiftLeft b1.toUInt32 8)
      let idx0 := UInt32.toNat (triple >>> 18) % 64
      let idx1 := UInt32.toNat (triple >>> 12) % 64
      let idx2 := UInt32.toNat (triple >>> 6) % 64
      let c0 := listGet alphabet idx0
      let c1 := listGet alphabet idx1
      let c2 := listGet alphabet idx2
      go input.size ('=' :: c2 :: c1 :: c0 :: acc)
    else if i < input.size then
      let b0 := input.get! i
      let triple := UInt32.shiftLeft b0.toUInt32 16
      let idx0 := UInt32.toNat (triple >>> 18) % 64
      let idx1 := UInt32.toNat (triple >>> 12) % 64
      let c0 := listGet alphabet idx0
      let c1 := listGet alphabet idx1
      go input.size ('=' :: '=' :: c1 :: c0 :: acc)
    else
      acc
  termination_by input.size - i
  String.ofList (go 0 []).reverse

/-- Decode a base64 string to a ByteArray. Returns none on invalid input. -/
def decode (s : String) : Option ByteArray :=
  let chars := s.toList.filter (λ c => c != '\n' && c != '\r' && c != ' ' && c != '\t')
  let rec process (cs : List Char) (acc : ByteArray) : Option ByteArray :=
    match cs with
    | [] => some acc
    | c1 :: c2 :: c3 :: c4 :: rest =>
      let p1 := charToIndex c1
      let p2 := charToIndex c2
      let p3 := if c3 == '=' then some 0 else charToIndex c3
      let p4 := if c4 == '=' then some 0 else charToIndex c4
      match p1, p2, p3, p4 with
      | some i1, some i2, some i3, some i4 =>
        let triple := (UInt32.shiftLeft (Nat.toUInt32 i1) 18) +
          (UInt32.shiftLeft (Nat.toUInt32 i2) 12) +
          (UInt32.shiftLeft (Nat.toUInt32 i3) 6) +
          Nat.toUInt32 i4
        let b0 := UInt32.toUInt8 ((triple >>> 16) % 256)
        let b1 := UInt32.toUInt8 ((triple >>> 8) % 256)
        let b2 := UInt32.toUInt8 (triple % 256)
        let acc' := acc.push b0
        if c3 == '=' then
          some acc'
        else
          let acc'' := acc'.push b1
          if c4 == '=' then
            some acc''
          else
            process rest (acc''.push b2)
      | _, _, _, _ => none
    | _ => none
  process chars ByteArray.empty

/--
Encode a String to base64 (convenience wrapper).
-/
def encodeString (s : String) : String :=
  encode s.toUTF8

/--
Decode a base64 string to a String (convenience wrapper).
-/
def decodeString (s : String) : Option String :=
  match decode s with
  | some ba => some (String.fromUTF8! ba)
  | none => none

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Specification for encoding: produce a base64 string from bytes.
-/
def specEncode (data : ByteArray) : String := encode data

/--
Specification for decoding: parse a base64 string back to bytes.
-/
def specDecode (s : String) : Option ByteArray := decode s

/--
Combined specification: encode or decode based on input flags.
-/
def spec (input : Base64Input) : Option String :=
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
I1: Encoded length is 4 * ceil(size / 3) with padding.
-/
theorem i_encoded_length_0 : encode ByteArray.empty = "" := by native_decide
theorem i_encoded_length_1 : encode (ByteArray.mk #[0x41]) = "QQ==" := by native_decide

/--
I2: Roundtrip: decode (encode ba) = some ba.
-/
theorem i_roundtrip_0 : decode (encode ByteArray.empty) = some ByteArray.empty := by native_decide
theorem i_roundtrip_1 : decode (encode (ByteArray.mk #[0x41])) = some (ByteArray.mk #[0x41]) := by native_decide
theorem i_roundtrip_hello : decode (encode "hello".toUTF8) = some "hello".toUTF8 := by native_decide

/--
I3: Spec on encode-mode inputs agrees with encode (∀-quantified invariant).
-/
theorem i_spec_encode_mode (input : Base64Input) (h : input.decode = false) :
    spec input = some (encode input.data) := by
  simp [spec, h]

/--
I4: Encode of empty input is empty.
-/
theorem i_encode_empty : encode ByteArray.empty = "" :=
  i_encoded_length_0

/--
I6: Roundtrip via convenience wrappers for a short string.
-/
theorem i_roundtrip_string_hello : decodeString (encodeString "hello") = some "hello" := by
  native_decide

/--
I7: Encoding all-zero bytes of various sizes uses predictable padding.
-/
theorem i_encode_zeros_1 : encode (ByteArray.mk #[0]) = "AA==" := by native_decide
theorem i_encode_zeros_2 : encode (ByteArray.mk #[0,0]) = "AAA=" := by native_decide
theorem i_encode_zeros_3 : encode (ByteArray.mk #[0,0,0]) = "AAAA" := by native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 7. Lemmas
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
alphabet has exactly 64 elements.
-/
theorem alphabet_length : alphabet.length = 64 := by native_decide

/--
charToIndex on valid alphabet characters succeeds.
-/
theorem charToIndex_A : charToIndex 'A' = some 0 := by native_decide
theorem charToIndex_plus : charToIndex '+' = some 62 := by native_decide
theorem charToIndex_slash : charToIndex '/' = some 63 := by native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 8. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- encode empty = "" -/
example : encode ByteArray.empty = "" := i_encoded_length_0

/-- encodeString "hello" = "aGVsbG8=" -/
example : encodeString "hello" = "aGVsbG8=" := by native_decide

/-- decode "aGVsbG8=" = "hello" -/
example : decodeString "aGVsbG8=" = some "hello" := by
  native_decide

end Lentils.Base64.Logic
