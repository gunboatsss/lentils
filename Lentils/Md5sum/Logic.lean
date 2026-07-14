/-
Md5sum.Logic — Pure MD5 hash implementation. 0BSD
-/

namespace Lentils.Md5sum.Logic

open ByteArray

-- ─── Helpers ──────────────────────────────────────────────────────────────────

def arrGet (arr : Array UInt32) (i : Nat) : UInt32 :=
  if h : i < arr.size then arr[i] else 0

def byteGet (arr : ByteArray) (i : Nat) : UInt8 :=
  if h : i < arr.size then arr[i] else 0

def rotl (x : UInt32) (n : UInt32) : UInt32 :=
  (x <<< n) ||| (x >>> (32 - n))

-- ─── Non-linear functions ────────────────────────────────────────────────────

def F (x y z : UInt32) : UInt32 := (x &&& y) ||| ((~~~x) &&& z)
def G (x y z : UInt32) : UInt32 := (x &&& z) ||| (y &&& (~~~z))
def H (x y z : UInt32) : UInt32 := x ^^^ y ^^^ z
def I (x y z : UInt32) : UInt32 := y ^^^ (x ||| (~~~z))

-- ─── Constants ────────────────────────────────────────────────────────────────

def shifts : Array UInt32 := #[
  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21]

def getT (i : Nat) : UInt32 :=
  let table : Array UInt32 := #[
    0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
    0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
    0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
    0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
    0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
    0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
    0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
    0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
    0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
    0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
    0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
    0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
    0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
    0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
    0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
    0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391]
  arrGet table i

-- ─── Padding ──────────────────────────────────────────────────────────────────

def md5Pad (data : ByteArray) : ByteArray :=
  let origLen := data.size
  let bitLen : UInt64 := UInt64.ofNat (origLen * 8)
  let data1 := data ++ ByteArray.mk (List.toArray [0x80])
  let padZeros := (120 - data1.size % 64) % 64
  let zeros : Array UInt8 := List.toArray (List.replicate padZeros 0)
  let data2 := data1 ++ ByteArray.mk zeros
  -- Build 8 LE bytes of bitLen
  let b0 := ((bitLen >>> 0).land 0xFF).toUInt8
  let b1 := ((bitLen >>> 8).land 0xFF).toUInt8
  let b2 := ((bitLen >>> 16).land 0xFF).toUInt8
  let b3 := ((bitLen >>> 24).land 0xFF).toUInt8
  let b4 := ((bitLen >>> 32).land 0xFF).toUInt8
  let b5 := ((bitLen >>> 40).land 0xFF).toUInt8
  let b6 := ((bitLen >>> 48).land 0xFF).toUInt8
  let b7 := ((bitLen >>> 56).land 0xFF).toUInt8
  let lenBytes : Array UInt8 := List.toArray [b0, b1, b2, b3, b4, b5, b6, b7]
  data2 ++ ByteArray.mk lenBytes

def readLE32 (block : ByteArray) (offset : Nat) : UInt32 :=
  let b0 := byteGet block offset
  let b1 := byteGet block (offset+1)
  let b2 := byteGet block (offset+2)
  let b3 := byteGet block (offset+3)
  (UInt32.ofNat b0.toNat) |||
  ((UInt32.ofNat b1.toNat) <<< 8) |||
  ((UInt32.ofNat b2.toNat) <<< 16) |||
  ((UInt32.ofNat b3.toNat) <<< 24)

def readWords (block : ByteArray) : Array UInt32 :=
  List.toArray (List.range 16 |>.map (λ i => readLE32 block (i * 4)))

-- ─── Block processing ──────────────────────────────────────────────────────────

def processBlock (state : UInt32 × UInt32 × UInt32 × UInt32) (block : ByteArray) : UInt32 × UInt32 × UInt32 × UInt32 :=
  let (a0, b0, c0, d0) := state
  let words := readWords block
  let rec go (a b c d : UInt32) (i : Nat) : UInt32 × UInt32 × UInt32 × UInt32 :=
    if i ≥ 64 then (a0 + a, b0 + b, c0 + c, d0 + d)
    else
      let f : UInt32 :=
        if i < 16 then F b c d
        else if i < 32 then G b c d
        else if i < 48 then H b c d
        else I b c d
      let g : Nat :=
        if i < 16 then i
        else if i < 32 then (5 * i + 1) % 16
        else if i < 48 then (3 * i + 5) % 16
        else (7 * i) % 16
      let temp := d
      let d := c
      let c := b
      let b := b + rotl (a + f + arrGet words g + getT i) (arrGet shifts i)
      let a := temp
      go a b c d (i + 1)
  go a0 b0 c0 d0 0

-- ─── Main hash ────────────────────────────────────────────────────────────────

def md5 (data : ByteArray) : ByteArray :=
  let padded := md5Pad data
  let numBlocks := padded.size / 64
  let initState : UInt32 × UInt32 × UInt32 × UInt32 :=
    (0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476)
  let rec processAll (state : UInt32 × UInt32 × UInt32 × UInt32) (i : Nat) : UInt32 × UInt32 × UInt32 × UInt32 :=
    if i ≥ numBlocks then state
    else
      let block := padded.extract (i * 64) ((i + 1) * 64)
      processAll (processBlock state block) (i + 1)
  let (a, b, c, d) := processAll initState 0
  let encodeWord (w : UInt32) : List UInt8 :=
    [((w >>> 0).land 0xFF).toUInt8,
     ((w >>> 8).land 0xFF).toUInt8,
     ((w >>> 16).land 0xFF).toUInt8,
     ((w >>> 24).land 0xFF).toUInt8]
  ByteArray.mk (List.toArray (encodeWord a ++ encodeWord b ++ encodeWord c ++ encodeWord d))

-- ─── Formatting ────────────────────────────────────────────────────────────

def formatHex (hash : ByteArray) : String :=
  String.ofList (List.flatten (hash.toList.map (λ b =>
    let hi := b.shiftRight 4
    let lo := b.land 0x0F
    [Char.ofNat (if hi < 10 then 0x30 + hi.toNat else 0x57 + hi.toNat),
     Char.ofNat (if lo < 10 then 0x30 + lo.toNat else 0x57 + lo.toNat)]
  )))

def formatStdin (data : ByteArray) : String :=
  formatHex (md5 data) ++ "  -\n"

-- ─── Proofs ──────────────────────────────────────────────────────────────────

-- Main hash test vector
example : md5 ByteArray.empty = ByteArray.mk (List.toArray
  ([0xd4, 0x1d, 0x8c, 0xd9, 0x8f, 0x00, 0xb2, 0x04,
    0xe9, 0x80, 0x09, 0x98, 0xec, 0xf8, 0x42, 0x7e] : List UInt8)) := by native_decide

-- ─── Intermediate Function Proofs ─────────────────────────────────────────────

-- Rotation helper proofs (rotate left)
example : rotl (0x00000001 : UInt32) 1 = (0x00000002 : UInt32) := by native_decide
example : rotl (0x80000000 : UInt32) 1 = (0x00000001 : UInt32) := by native_decide
example : rotl (0xFFFFFFFF : UInt32) 32 = (0xFFFFFFFF : UInt32) := by native_decide

-- MD5 non-linear function proofs
-- F(x,y,z) = (x AND y) OR ((NOT x) AND z)
example : F (0xFFFFFFFF : UInt32) (0xFFFFFFFF : UInt32) (0 : UInt32) = (0xFFFFFFFF : UInt32) := by native_decide
example : F (0 : UInt32) (0xFFFFFFFF : UInt32) (0xFFFFFFFF : UInt32) = (0xFFFFFFFF : UInt32) := by native_decide
example : F (0 : UInt32) (0 : UInt32) (0xFFFFFFFF : UInt32) = (0xFFFFFFFF : UInt32) := by native_decide

-- G(x,y,z) = (x AND z) OR (y AND (NOT z))
example : G (0xFFFFFFFF : UInt32) (0 : UInt32) (0xFFFFFFFF : UInt32) = (0xFFFFFFFF : UInt32) := by native_decide
example : G (0 : UInt32) (0xFFFFFFFF : UInt32) (0xFFFFFFFF : UInt32) = (0 : UInt32) := by native_decide

-- H(x,y,z) = x XOR y XOR z
example : H (0xFFFFFFFF : UInt32) (0xFFFFFFFF : UInt32) (0xFFFFFFFF : UInt32) = (0xFFFFFFFF : UInt32) := by native_decide
example : H (0xAAAAAAAA : UInt32) (0x55555555 : UInt32) (0xFFFFFFFF : UInt32) = (0 : UInt32) := by native_decide

-- I(x,y,z) = y XOR (x OR (NOT z))
example : I (0xFFFFFFFF : UInt32) (0xFFFFFFFF : UInt32) (0 : UInt32) = (0 : UInt32) := by native_decide

-- Padding proofs
example : (md5Pad ByteArray.empty).size = 64 := by native_decide  -- Minimum 1 block
example : (md5Pad "abc".toUTF8).size = 64 := by native_decide       -- Fits in one block

-- Format hex proof
example : formatHex ByteArray.empty = "" := by native_decide

end Lentils.Md5sum.Logic
