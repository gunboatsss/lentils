/-
Sha512sum.Logic — Pure SHA-512 hash implementation. 0BSD
-/

namespace Lentils.Sha512sum.Logic

open ByteArray

-- ─── Helpers ──────────────────────────────────────────────────────────────────

def arrGet (arr : Array UInt64) (i : Nat) : UInt64 :=
  if h : i < arr.size then arr[i] else 0

def byteGet (arr : ByteArray) (i : Nat) : UInt8 :=
  if h : i < arr.size then arr[i] else 0

def rotr (x : UInt64) (n : UInt64) : UInt64 :=
  (x >>> n) ||| (x <<< (64 - n))

-- ─── Constants ────────────────────────────────────────────────────────────────

def initH : Array UInt64 := #[
  0x6a09e667f3bcc908, 0xbb67ae8584caa73b, 0x3c6ef372fe94f82b, 0xa54ff53a5f1d36f1,
  0x510e527fade682d1, 0x9b05688c2b3e6c1f, 0x1f83d9abfb41bd6b, 0x5be0cd19137e2179]

-- SHA-384 uses the same algorithm but different initial values
def initH384 : Array UInt64 := #[
  0xcbbb9d5dc1059ed8, 0x629a292a367cd507, 0x9159015a3070dd17, 0x152fecd8f70e5939,
  0x67332667ffc00b31, 0x8eb44a8768581511, 0xdb0c2e0d64f98fa7, 0x47b5481dbefa4fa4]

def getK (i : Nat) : UInt64 :=
  let table : Array UInt64 := #[
    0x428a2f98d728ae22, 0x7137449123ef65cd, 0xb5c0fbcfec4d3b2f, 0xe9b5dba58189dbbc,
    0x3956c25bf348b538, 0x59f111f1b605d019, 0x923f82a4af194f9b, 0xab1c5ed5da6d8118,
    0xd807aa98a3030242, 0x12835b0145706fbe, 0x243185be4ee4b28c, 0x550c7dc3d5ffb4e2,
    0x72be5d74f27b896f, 0x80deb1fe3b1696b1, 0x9bdc06a725c71235, 0xc19bf174cf692694,
    0xe49b69c19ef14ad2, 0xefbe4786384f25e3, 0x0fc19dc68b8cd5b5, 0x240ca1cc77ac9c65,
    0x2de92c6f592b0275, 0x4a7484aa6ea6e483, 0x5cb0a9dcbd41fbd4, 0x76f988da831153b5,
    0x983e5152ee66dfab, 0xa831c66d2db43210, 0xb00327c898fb213f, 0xbf597fc7beef0ee4,
    0xc6e00bf33da88fc2, 0xd5a79147930aa725, 0x06ca6351e003826f, 0x142929670a0e6e70,
    0x27b70a8546d22ffc, 0x2e1b21385c26c926, 0x4d2c6dfc5ac42aed, 0x53380d139d95b3df,
    0x650a73548baf63de, 0x766a0abb3c77b2a8, 0x81c2c92e47edaee6, 0x92722c851482353b,
    0xa2bfe8a14cf10364, 0xa81a664bbc423001, 0xc24b8b70d0f89791, 0xc76c51a30654be30,
    0xd192e819d6ef5218, 0xd69906245565a910, 0xf40e35855771202a, 0x106aa07032bbd1b8,
    0x19a4c116b8d2d0c8, 0x1e376c085141ab53, 0x2748774cdf8eeb99, 0x34b0bcb5e19b48a8,
    0x391c0cb3c5c95a63, 0x4ed8aa4ae3418acb, 0x5b9cca4f7763e373, 0x682e6ff3d6b2b8a3,
    0x748f82ee5defb2fc, 0x78a5636f43172f60, 0x84c87814a1f0ab72, 0x8cc702081a6439ec,
    0x90befffa23631e28, 0xa4506cebde82bde9, 0xbef9a3f7b2c67915, 0xc67178f2e372532b,
    0xca273eceea26619c, 0xd186b8c721c0c207, 0xeada7dd6cde0eb1e, 0xf57d4f7fee6ed178,
    0x06f067aa72176fba, 0x0a637dc5a2c898a6, 0x113f9804bef90dae, 0x1b710b35131c471b,
    0x28db77f523047d84, 0x32caab7b40c72493, 0x3c9ebe0a15c9bebc, 0x431d67c49c100d4c,
    0x4cc5d4becb3e42b6, 0x597f299cfc657e2a, 0x5fcb6fab3ad6faec, 0x6c44198c4a475817]
  arrGet table i

-- ─── Functions ────────────────────────────────────────────────────────────────

def Sigma0 (x : UInt64) : UInt64 := rotr x 28 ^^^ rotr x 34 ^^^ rotr x 39
def Sigma1 (x : UInt64) : UInt64 := rotr x 14 ^^^ rotr x 18 ^^^ rotr x 41
def sigma0 (x : UInt64) : UInt64 := rotr x 1 ^^^ rotr x 8 ^^^ (x >>> 7)
def sigma1 (x : UInt64) : UInt64 := rotr x 19 ^^^ rotr x 61 ^^^ (x >>> 6)

def Ch (x y z : UInt64) : UInt64 := (x &&& y) ^^^ ((~~~x) &&& z)
def Maj (x y z : UInt64) : UInt64 := (x &&& y) ^^^ (x &&& z) ^^^ (y &&& z)

-- ─── Padding: 128-bit big-endian bit-length ─────────────────────────────────

def sha512Pad (data : ByteArray) : ByteArray :=
  let origLen := data.size
  let origLen64 := UInt64.ofNat origLen
  let data1 := data ++ ByteArray.mk (List.toArray [0x80])
  let padZeros := (240 - data1.size % 128) % 128
  let zeros : Array UInt8 := List.toArray (List.replicate padZeros 0)
  let data2 := data1 ++ ByteArray.mk zeros
  -- Compute 128-bit bit length: high 64 bits, low 64 bits (big-endian)
  let bitLenLow := origLen64 * 8
  let bitLenHigh : UInt64 := 0  -- for inputs < 2^61 bytes, high word is 0
  let hb0 := ((bitLenHigh >>> 56).land 0xFF).toUInt8
  let hb1 := ((bitLenHigh >>> 48).land 0xFF).toUInt8
  let hb2 := ((bitLenHigh >>> 40).land 0xFF).toUInt8
  let hb3 := ((bitLenHigh >>> 32).land 0xFF).toUInt8
  let hb4 := ((bitLenHigh >>> 24).land 0xFF).toUInt8
  let hb5 := ((bitLenHigh >>> 16).land 0xFF).toUInt8
  let hb6 := ((bitLenHigh >>> 8).land 0xFF).toUInt8
  let hb7 := ((bitLenHigh >>> 0).land 0xFF).toUInt8
  let lb0 := ((bitLenLow >>> 56).land 0xFF).toUInt8
  let lb1 := ((bitLenLow >>> 48).land 0xFF).toUInt8
  let lb2 := ((bitLenLow >>> 40).land 0xFF).toUInt8
  let lb3 := ((bitLenLow >>> 32).land 0xFF).toUInt8
  let lb4 := ((bitLenLow >>> 24).land 0xFF).toUInt8
  let lb5 := ((bitLenLow >>> 16).land 0xFF).toUInt8
  let lb6 := ((bitLenLow >>> 8).land 0xFF).toUInt8
  let lb7 := ((bitLenLow >>> 0).land 0xFF).toUInt8
  let lenBytes : Array UInt8 := List.toArray [hb0, hb1, hb2, hb3, hb4, hb5, hb6, hb7, lb0, lb1, lb2, lb3, lb4, lb5, lb6, lb7]
  data2 ++ ByteArray.mk lenBytes

-- ─── Block processing ─────────────────────────────────────────────────────────

def readBE64 (block : ByteArray) (offset : Nat) : UInt64 :=
  let b0 := byteGet block offset
  let b1 := byteGet block (offset+1)
  let b2 := byteGet block (offset+2)
  let b3 := byteGet block (offset+3)
  let b4 := byteGet block (offset+4)
  let b5 := byteGet block (offset+5)
  let b6 := byteGet block (offset+6)
  let b7 := byteGet block (offset+7)
  ((UInt64.ofNat b0.toNat) <<< 56) |||
  ((UInt64.ofNat b1.toNat) <<< 48) |||
  ((UInt64.ofNat b2.toNat) <<< 40) |||
  ((UInt64.ofNat b3.toNat) <<< 32) |||
  ((UInt64.ofNat b4.toNat) <<< 24) |||
  ((UInt64.ofNat b5.toNat) <<< 16) |||
  ((UInt64.ofNat b6.toNat) <<< 8) |||
  (UInt64.ofNat b7.toNat)

def readWords (block : ByteArray) : Array UInt64 :=
  List.toArray (List.range 16 |>.map (λ i => readBE64 block (i * 8)))

def expandWords (w16 : Array UInt64) : Array UInt64 :=
  let rec expand (arr : Array UInt64) (i : Nat) : Array UInt64 :=
    if i ≥ 80 then arr
    else
      let s0 := sigma0 (arrGet arr (i-15))
      let s1 := sigma1 (arrGet arr (i-2))
      let val := arrGet arr (i-16) + s0 + arrGet arr (i-7) + s1
      expand (arr.push val) (i + 1)
  expand w16 16

def processBlock (state : Array UInt64) (block : ByteArray) : Array UInt64 :=
  let w := expandWords (readWords block)
  let rec go (a b c d e f g h : UInt64) (i : Nat) : UInt64 × UInt64 × UInt64 × UInt64 × UInt64 × UInt64 × UInt64 × UInt64 :=
    if i ≥ 80 then (a, b, c, d, e, f, g, h)
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
Generic SHA-512-family hash with configurable initial value and output word count.
Used by both SHA-512 (8 words) and SHA-384 (6 words).
-/
def sha512WithInit (hashInit : Array UInt64) (numWords : Nat) (data : ByteArray) : ByteArray :=
  let padded := sha512Pad data
  let numBlocks := padded.size / 128
  let rec processAll (state : Array UInt64) (i : Nat) : Array UInt64 :=
    if i ≥ numBlocks then state
    else
      let block := padded.extract (i * 128) ((i + 1) * 128)
      processAll (processBlock state block) (i + 1)
  let state := processAll hashInit 0
  let encodeWord (w : UInt64) : List UInt8 :=
    [((w >>> 56).land 0xFF).toUInt8,
     ((w >>> 48).land 0xFF).toUInt8,
     ((w >>> 40).land 0xFF).toUInt8,
     ((w >>> 32).land 0xFF).toUInt8,
     ((w >>> 24).land 0xFF).toUInt8,
     ((w >>> 16).land 0xFF).toUInt8,
     ((w >>> 8).land 0xFF).toUInt8,
     ((w >>> 0).land 0xFF).toUInt8]
  let allWords := List.range numWords |>.foldl (λ acc i => acc ++ encodeWord (arrGet state i)) []
  ByteArray.mk (List.toArray allWords)

def sha512 (data : ByteArray) : ByteArray :=
  sha512WithInit initH 8 data

def sha384 (data : ByteArray) : ByteArray :=
  sha512WithInit initH384 6 data

-- ─── Formatting ────────────────────────────────────────────────────────────

def formatHex (hash : ByteArray) : String :=
  String.ofList (List.flatten (hash.toList.map (λ b =>
    let hi := b.shiftRight 4
    let lo := b.land 0x0F
    [Char.ofNat (if hi < 10 then 0x30 + hi.toNat else 0x57 + hi.toNat),
     Char.ofNat (if lo < 10 then 0x30 + lo.toNat else 0x57 + lo.toNat)]
  )))

def formatStdin (data : ByteArray) : String :=
  formatHex (sha512 data) ++ "  -\n"

-- ─── Proofs ──────────────────────────────────────────────────────────────────

-- Main hash test vector
example : sha512 ByteArray.empty = ByteArray.mk (List.toArray
  ([0xcf, 0x83, 0xe1, 0x35, 0x7e, 0xef, 0xb8, 0xbd,
    0xf1, 0x54, 0x28, 0x50, 0xd6, 0x6d, 0x80, 0x07,
    0xd6, 0x20, 0xe4, 0x05, 0x0b, 0x57, 0x15, 0xdc,
    0x83, 0xf4, 0xa9, 0x21, 0xd3, 0x6c, 0xe9, 0xce,
    0x47, 0xd0, 0xd1, 0x3c, 0x5d, 0x85, 0xf2, 0xb0,
    0xff, 0x83, 0x18, 0xd2, 0x87, 0x7e, 0xec, 0x2f,
    0x63, 0xb9, 0x31, 0xbd, 0x47, 0x41, 0x7a, 0x81,
    0xa5, 0x38, 0x32, 0x7a, 0xf9, 0x27, 0xda, 0x3e] : List UInt8)) := by native_decide

-- ─── Intermediate Function Proofs ─────────────────────────────────────────────

-- Padding proofs
example : (sha512Pad ByteArray.empty).size = 128 := by native_decide  -- Minimum 1 block (larger than SHA-256)
example : (sha512Pad "abc".toUTF8).size = 128 := by native_decide      -- Fits in one block

-- Rotation helper proofs (64-bit)
example : rotr (0x0000000000000001 : UInt64) 1 = (0x8000000000000000 : UInt64) := by native_decide
example : rotr (0x8000000000000000 : UInt64) 1 = (0x4000000000000000 : UInt64) := by native_decide
example : rotr (0xFFFFFFFFFFFFFFFF : UInt64) 64 = (0xFFFFFFFFFFFFFFFF : UInt64) := by native_decide

-- Sigma function proofs (64-bit) - simple identity case
example : Sigma0 (0 : UInt64) = (0 : UInt64) := by native_decide
example : Sigma1 (0 : UInt64) = (0 : UInt64) := by native_decide

-- Ch function proofs: Ch(x,y,z) = (x AND y) XOR ((NOT x) AND z)
example : Ch (0xFFFFFFFFFFFFFFFF : UInt64) (0xFFFFFFFFFFFFFFFF : UInt64) (0 : UInt64) = (0xFFFFFFFFFFFFFFFF : UInt64) := by native_decide
example : Ch (0 : UInt64) (0xFFFFFFFFFFFFFFFF : UInt64) (0xFFFFFFFFFFFFFFFF : UInt64) = (0xFFFFFFFFFFFFFFFF : UInt64) := by native_decide

-- Maj function proofs
example : Maj (0xFFFFFFFFFFFFFFFF : UInt64) (0 : UInt64) (0 : UInt64) = (0 : UInt64) := by native_decide

-- Message expansion proof (expands to 80 words)
example : (expandWords (readWords (sha512Pad "abc".toUTF8))).size = 80 := by native_decide

-- Format hex proof
example : formatHex ByteArray.empty = "" := by native_decide

end Lentils.Sha512sum.Logic
