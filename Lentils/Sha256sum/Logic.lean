/-
Sha256sum.Logic — Pure SHA-256 hash implementation. 0BSD
-/

namespace Lentils.Sha256sum.Logic

open ByteArray

-- ─── Helpers ──────────────────────────────────────────────────────────────────

def arrGet (arr : Array UInt32) (i : Nat) : UInt32 :=
  if h : i < arr.size then arr[i] else 0

def byteGet (arr : ByteArray) (i : Nat) : UInt8 :=
  if h : i < arr.size then arr[i] else 0

def rotr (x : UInt32) (n : UInt32) : UInt32 :=
  (x >>> n) ||| (x <<< (32 - n))

-- ─── Constants ────────────────────────────────────────────────────────────────

def initH : Array UInt32 := #[
  0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
  0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19]

-- SHA-224 uses the same algorithm but different initial values
def initH224 : Array UInt32 := #[
  0xc1059ed8, 0x367cd507, 0x3070dd17, 0xf70e5939,
  0xffc00b31, 0x68581511, 0x64f98fa7, 0xbefa4fa4]

def getK (i : Nat) : UInt32 :=
  let table : Array UInt32 := #[
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
    0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
    0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
    0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
    0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2]
  arrGet table i

-- ─── Functions ────────────────────────────────────────────────────────────────

def Sigma0 (x : UInt32) : UInt32 := rotr x 2 ^^^ rotr x 13 ^^^ rotr x 22
def Sigma1 (x : UInt32) : UInt32 := rotr x 6 ^^^ rotr x 11 ^^^ rotr x 25
def sigma0 (x : UInt32) : UInt32 := rotr x 7 ^^^ rotr x 18 ^^^ (x >>> 3)
def sigma1 (x : UInt32) : UInt32 := rotr x 17 ^^^ rotr x 19 ^^^ (x >>> 10)

def Ch (x y z : UInt32) : UInt32 := (x &&& y) ^^^ ((~~~x) &&& z)
def Maj (x y z : UInt32) : UInt32 := (x &&& y) ^^^ (x &&& z) ^^^ (y &&& z)

-- ─── Padding ──────────────────────────────────────────────────────────────────

def sha256Pad (data : ByteArray) : ByteArray :=
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

-- ─── Block processing ─────────────────────────────────────────────────────────

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
    if i ≥ 64 then arr
    else
      let s0 := sigma0 (arrGet arr (i-15))
      let s1 := sigma1 (arrGet arr (i-2))
      let val := arrGet arr (i-16) + s0 + arrGet arr (i-7) + s1
      expand (arr.push val) (i + 1)
  expand w16 16

def processBlock (state : Array UInt32) (block : ByteArray) : Array UInt32 :=
  let w := expandWords (readWords block)
  let rec go (a b c d e f g h : UInt32) (i : Nat) : UInt32 × UInt32 × UInt32 × UInt32 × UInt32 × UInt32 × UInt32 × UInt32 :=
    if i ≥ 64 then (a, b, c, d, e, f, g, h)
    else
      let S1 := Sigma1 e
      let ch := Ch e f g
      let temp1 := h + S1 + ch + getK i + arrGet w i
      let S0 := Sigma0 a
      let maj := Maj a b c
      let temp2 := S0 + maj
      go (temp1 + temp2) a b c (d + temp1) e f g (i + 1)
  let a0 := arrGet state 0
  let b0 := arrGet state 1
  let c0 := arrGet state 2
  let d0 := arrGet state 3
  let e0 := arrGet state 4
  let f0 := arrGet state 5
  let g0 := arrGet state 6
  let h0 := arrGet state 7
  let (a, b, c, d, e, f, g, h) := go a0 b0 c0 d0 e0 f0 g0 h0 0
  List.toArray [a0 + a, b0 + b, c0 + c, d0 + d, e0 + e, f0 + f, g0 + g, h0 + h]

-- ─── Main hash ────────────────────────────────────────────────────────────────

/--
Generic SHA-256-family hash with configurable initial value and output word count.
Used by both SHA-256 (8 words) and SHA-224 (7 words).
-/
def sha256WithInit (hashInit : Array UInt32) (numWords : Nat) (data : ByteArray) : ByteArray :=
  let padded := sha256Pad data
  let numBlocks := padded.size / 64
  let rec processAll (state : Array UInt32) (i : Nat) : Array UInt32 :=
    if i ≥ numBlocks then state
    else
      let block := padded.extract (i * 64) ((i + 1) * 64)
      processAll (processBlock state block) (i + 1)
  let state := processAll hashInit 0
  let encodeWord (w : UInt32) : List UInt8 :=
    [((w >>> 24).land 0xFF).toUInt8,
     ((w >>> 16).land 0xFF).toUInt8,
     ((w >>> 8).land 0xFF).toUInt8,
     ((w >>> 0).land 0xFF).toUInt8]
  let allWords := List.range numWords |>.foldl (λ acc i => acc ++ encodeWord (arrGet state i)) []
  ByteArray.mk (List.toArray allWords)

def sha256 (data : ByteArray) : ByteArray :=
  sha256WithInit initH 8 data

def sha224 (data : ByteArray) : ByteArray :=
  sha256WithInit initH224 7 data

-- ─── Formatting ────────────────────────────────────────────────────────────

def formatHex (hash : ByteArray) : String :=
  String.ofList (List.flatten (hash.toList.map (λ b =>
    let hi := b.shiftRight 4
    let lo := b.land 0x0F
    [Char.ofNat (if hi < 10 then 0x30 + hi.toNat else 0x57 + hi.toNat),
     Char.ofNat (if lo < 10 then 0x30 + lo.toNat else 0x57 + lo.toNat)]
  )))

def formatStdin (data : ByteArray) : String :=
  formatHex (sha256 data) ++ "  -\n"

-- ─── Proofs ──────────────────────────────────────────────────────────────────

-- Main hash test vector
example : sha256 ByteArray.empty = ByteArray.mk (List.toArray
  ([0xe3, 0xb0, 0xc4, 0x42, 0x98, 0xfc, 0x1c, 0x14,
    0x9a, 0xfb, 0xf4, 0xc8, 0x99, 0x6f, 0xb9, 0x24,
    0x27, 0xae, 0x41, 0xe4, 0x64, 0x9b, 0x93, 0x4c,
    0xa4, 0x95, 0x99, 0x1b, 0x78, 0x52, 0xb8, 0x55] : List UInt8)) := by native_decide

-- ─── Intermediate Function Proofs ─────────────────────────────────────────────

-- Padding proofs
example : (sha256Pad ByteArray.empty).size = 64 := by native_decide  -- Minimum 1 block
example : (sha256Pad "abc".toUTF8).size = 64 := by native_decide       -- Fits in one block
example : (sha256Pad (ByteArray.mk (List.toArray (List.replicate 55 0x41)))).size = 64 := by native_decide

-- Rotation helper proofs
example : rotr (0x00000001 : UInt32) 1 = (0x80000000 : UInt32) := by native_decide
example : rotr (0x80000000 : UInt32) 1 = (0x40000000 : UInt32) := by native_decide
example : rotr (0xFFFFFFFF : UInt32) 32 = (0xFFFFFFFF : UInt32) := by native_decide

-- Sigma function proofs (simple identity cases)
example : Sigma0 (0 : UInt32) = (0 : UInt32) := by native_decide
example : Sigma1 (0 : UInt32) = (0 : UInt32) := by native_decide

-- Ch and Maj function proofs
example : Ch (0xFFFFFFFF : UInt32) (0xFFFFFFFF : UInt32) (0 : UInt32) = (0xFFFFFFFF : UInt32) := by native_decide
example : Ch (0 : UInt32) (0xFFFFFFFF : UInt32) (0xFFFFFFFF : UInt32) = (0xFFFFFFFF : UInt32) := by native_decide
example : Maj (0xFFFFFFFF : UInt32) (0 : UInt32) (0 : UInt32) = (0 : UInt32) := by native_decide

-- Message expansion proof (expands to 64 words)
example : (expandWords (readWords (sha256Pad "abc".toUTF8))).size = 64 := by native_decide

-- Format hex proof
example : formatHex ByteArray.empty = "" := by native_decide

end Lentils.Sha256sum.Logic
