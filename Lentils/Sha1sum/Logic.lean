/-
Sha1sum.Logic — Pure SHA-1 hash implementation. 0BSD
-/

namespace Lentils.Sha1sum.Logic

open ByteArray

-- ─── Helpers ──────────────────────────────────────────────────────────────────

def arrGet (arr : Array UInt32) (i : Nat) : UInt32 :=
  if h : i < arr.size then arr[i] else 0

def byteGet (arr : ByteArray) (i : Nat) : UInt8 :=
  if h : i < arr.size then arr[i] else 0

def rotl (x : UInt32) (n : UInt32) : UInt32 :=
  (x <<< n) ||| (x >>> (32 - n))

-- ─── Round functions ────────────────────────────────────────────────────────

def f1 (b c d : UInt32) : UInt32 := (b &&& c) ||| ((~~~b) &&& d)
def f2 (b c d : UInt32) : UInt32 := b ^^^ c ^^^ d
def f3 (b c d : UInt32) : UInt32 := (b &&& c) ||| (b &&& d) ||| (c &&& d)

def roundFunc (i : Nat) (b c d : UInt32) : UInt32 :=
  if i < 20 then f1 b c d
  else if i < 40 then f2 b c d
  else if i < 60 then f3 b c d
  else f2 b c d

def roundConst (i : Nat) : UInt32 :=
  if i < 20 then 0x5a827999
  else if i < 40 then 0x6ed9eba1
  else if i < 60 then 0x8f1bbcdc
  else 0xca62c1d6

-- ─── Padding ──────────────────────────────────────────────────────────────────

def sha1Pad (data : ByteArray) : ByteArray :=
  let origLen := data.size
  let bitLen : UInt64 := UInt64.ofNat (origLen * 8)
  let data1 := data ++ ByteArray.mk (List.toArray [0x80])
  let padZeros := (120 - data1.size % 64) % 64
  let zeros : Array UInt8 := List.toArray (List.replicate padZeros 0)
  let data2 := data1 ++ ByteArray.mk zeros
  let b0 := ((bitLen >>> 56).land 0xFF).toUInt8
  let b1 := ((bitLen >>> 48).land 0xFF).toUInt8
  let b2 := ((bitLen >>> 40).land 0xFF).toUInt8
  let b3 := ((bitLen >>> 32).land 0xFF).toUInt8
  let b4 := ((bitLen >>> 24).land 0xFF).toUInt8
  let b5 := ((bitLen >>> 16).land 0xFF).toUInt8
  let b6 := ((bitLen >>> 8).land 0xFF).toUInt8
  let b7 := ((bitLen >>> 0).land 0xFF).toUInt8
  let lenBytes : Array UInt8 := List.toArray [b0, b1, b2, b3, b4, b5, b6, b7]
  data2 ++ ByteArray.mk lenBytes

-- ─── Block processing ──────────────────────────────────────────────────────────

def readBE32 (block : ByteArray) (offset : Nat) : UInt32 :=
  let b0 := byteGet block offset
  let b1 := byteGet block (offset+1)
  let b2 := byteGet block (offset+2)
  let b3 := byteGet block (offset+3)
  ((UInt32.ofNat b0.toNat) <<< 24) |||
  ((UInt32.ofNat b1.toNat) <<< 16) |||
  ((UInt32.ofNat b2.toNat) <<< 8) |||
  (UInt32.ofNat b3.toNat)

def readWords (block : ByteArray) : Array UInt32 :=
  List.toArray (List.range 16 |>.map (λ i => readBE32 block (i * 4)))

def expandWords (w16 : Array UInt32) : Array UInt32 :=
  let rec expand (arr : Array UInt32) (i : Nat) : Array UInt32 :=
    if i ≥ 80 then arr
    else
      let val := rotl (arrGet arr (i-3) ^^^ arrGet arr (i-8) ^^^ arrGet arr (i-14) ^^^ arrGet arr (i-16)) 1
      expand (arr.push val) (i + 1)
  expand w16 16

def processBlock (state : UInt32 × UInt32 × UInt32 × UInt32 × UInt32) (block : ByteArray) : UInt32 × UInt32 × UInt32 × UInt32 × UInt32 :=
  let (h0, h1, h2, h3, h4) := state
  let w := expandWords (readWords block)
  let rec go (a b c d e : UInt32) (i : Nat) : UInt32 × UInt32 × UInt32 × UInt32 × UInt32 :=
    if i ≥ 80 then (h0 + a, h1 + b, h2 + c, h3 + d, h4 + e)
    else
      let temp := rotl a 5 + roundFunc i b c d + e + roundConst i + arrGet w i
      go temp a (rotl b 30) c d (i + 1)
  go h0 h1 h2 h3 h4 0

-- ─── Main hash ────────────────────────────────────────────────────────────────

def sha1 (data : ByteArray) : ByteArray :=
  let padded := sha1Pad data
  let numBlocks := padded.size / 64
  let initState : UInt32 × UInt32 × UInt32 × UInt32 × UInt32 :=
    (0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476, 0xc3d2e1f0)
  let rec processAll (state : UInt32 × UInt32 × UInt32 × UInt32 × UInt32) (i : Nat) : UInt32 × UInt32 × UInt32 × UInt32 × UInt32 :=
    if i ≥ numBlocks then state
    else
      let block := padded.extract (i * 64) ((i + 1) * 64)
      processAll (processBlock state block) (i + 1)
  let (h0, h1, h2, h3, h4) := processAll initState 0
  let encodeWord (w : UInt32) : List UInt8 :=
    [((w >>> 24).land 0xFF).toUInt8,
     ((w >>> 16).land 0xFF).toUInt8,
     ((w >>> 8).land 0xFF).toUInt8,
     ((w >>> 0).land 0xFF).toUInt8]
  ByteArray.mk (List.toArray (encodeWord h0 ++ encodeWord h1 ++ encodeWord h2 ++ encodeWord h3 ++ encodeWord h4))

-- ─── Formatting ────────────────────────────────────────────────────────────

def formatHex (hash : ByteArray) : String :=
  String.ofList (List.flatten (hash.toList.map (λ b =>
    let hi := b.shiftRight 4
    let lo := b.land 0x0F
    [Char.ofNat (if hi < 10 then 0x30 + hi.toNat else 0x57 + hi.toNat),
     Char.ofNat (if lo < 10 then 0x30 + lo.toNat else 0x57 + lo.toNat)]
  )))

def formatStdin (data : ByteArray) : String :=
  formatHex (sha1 data) ++ "  -\n"

-- ─── Proofs ──────────────────────────────────────────────────────────────────

-- Main hash test vector
example : sha1 ByteArray.empty = ByteArray.mk (List.toArray
  ([0xda, 0x39, 0xa3, 0xee, 0x5e, 0x6b, 0x4b, 0x0d,
    0x32, 0x55, 0xbf, 0xef, 0x95, 0x60, 0x18, 0x90,
    0xaf, 0xd8, 0x07, 0x09] : List UInt8)) := by native_decide

-- ─── Intermediate Function Proofs ─────────────────────────────────────────────

-- Rotation helper proofs (rotate left)
example : rotl (0x00000001 : UInt32) 1 = (0x00000002 : UInt32) := by native_decide
example : rotl (0x80000000 : UInt32) 1 = (0x00000001 : UInt32) := by native_decide
example : rotl (0xFFFFFFFF : UInt32) 32 = (0xFFFFFFFF : UInt32) := by native_decide

-- Round function proofs
-- f1 (rounds 0-19): (b AND c) OR ((NOT b) AND d) = MAJ variant
example : f1 (0xFFFFFFFF : UInt32) (0xFFFFFFFF : UInt32) (0xFFFFFFFF : UInt32) = (0xFFFFFFFF : UInt32) := by native_decide
example : f1 (0 : UInt32) (0xFFFFFFFF : UInt32) (0xFFFFFFFF : UInt32) = (0xFFFFFFFF : UInt32) := by native_decide

-- f2 (rounds 20-39, 60-79): XOR
example : f2 (0xFFFFFFFF : UInt32) (0xFFFFFFFF : UInt32) (0xFFFFFFFF : UInt32) = (0xFFFFFFFF : UInt32) := by native_decide

-- f3 (rounds 40-59): majority
example : f3 (0xFFFFFFFF : UInt32) (0 : UInt32) (0 : UInt32) = (0 : UInt32) := by native_decide
example : f3 (0xFFFFFFFF : UInt32) (0xFFFFFFFF : UInt32) (0 : UInt32) = (0xFFFFFFFF : UInt32) := by native_decide

-- Round constant proofs
example : roundConst 0 = (0x5a827999 : UInt32) := by native_decide
example : roundConst 20 = (0x6ed9eba1 : UInt32) := by native_decide
example : roundConst 40 = (0x8f1bbcdc : UInt32) := by native_decide
example : roundConst 60 = (0xca62c1d6 : UInt32) := by native_decide

-- Padding proofs
example : (sha1Pad ByteArray.empty).size = 64 := by native_decide
example : (sha1Pad "abc".toUTF8).size = 64 := by native_decide

-- Format hex proof
example : formatHex ByteArray.empty = "" := by native_decide

end Lentils.Sha1sum.Logic
