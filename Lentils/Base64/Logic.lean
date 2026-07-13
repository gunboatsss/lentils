/-
Base64.Logic — Pure base64 encoding/decoding for `base64`. 0BSD
-/

namespace Lentils.Base64.Logic

/-- The base64 alphabet as a list of chars. -/
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

/-- Encode a ByteArray to a base64 string. Uses index recursion for termination. -/
partial def encode (input : ByteArray) : String :=
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

def encodeString (s : String) : String :=
  encode s.toUTF8

def decodeString (s : String) : Option String :=
  match decode s with
  | some ba => some (String.fromUTF8! ba)
  | none => none

end Lentils.Base64.Logic
