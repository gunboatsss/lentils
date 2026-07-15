/-
Base32.Logic — Pure base32 encoding/decoding for `base32`. 0BSD
-/

namespace Lentils.Base32.Logic

/-- The RFC 4648 base32 alphabet as a list of chars. -/
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

/-- Encode a ByteArray to a base32 string (RFC 4648). -/
partial def encode (input : ByteArray) : String :=
  let rec go (i : Nat) (acc : List Char) : List Char :=
    if i + 4 < input.size then
      -- Process 5 bytes → 40 bits → 8 chars
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
      -- 4 bytes → 7 chars + 1 pad
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
      -- 3 bytes → 5 chars + 3 pad
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
      -- 2 bytes → 4 chars + 4 pad
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
      -- 1 byte → 2 chars + 6 pad
      let b0 := input.get! i
      let q := UInt64.ofNat b0.toNat <<< 32
      let idx0 := (q >>> 35).toNat.land 0x1F
      let idx1 := (q >>> 30).toNat.land 0x1F
      go input.size ('=' :: '=' :: '=' :: '=' :: '=' :: '=' :: listGet alphabet idx1 ::
        listGet alphabet idx0 :: acc)
    else
      acc
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
        -- Determine how many pad chars
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
          | _ => ByteArray.empty  -- invalid padding)
        process rest (acc ++ bytesOut)
      | _, _, _, _, _, _, _, _ => none
    | _ => none  -- invalid length (not a multiple of 8)
  process chars ByteArray.empty

-- ─── Proofs ──────────────────────────────────────────────────────────────────

theorem encode_empty : encode ByteArray.empty = "" := by native_decide

theorem decode_empty : decode "" = some ByteArray.empty := by native_decide

theorem roundtrip_empty : decode (encode ByteArray.empty) = some ByteArray.empty := by native_decide

theorem encode_hello :
  encode "hello".toUTF8 = "NBSWY3DP" := by native_decide

theorem decode_NBSWY3DP :
  decode "NBSWY3DP" = some "hello".toUTF8 := by native_decide

theorem roundtrip_hello :
  decode (encode "hello".toUTF8) = some "hello".toUTF8 := by native_decide

end Lentils.Base32.Logic
