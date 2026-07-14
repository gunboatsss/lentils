set_option maxRecDepth 100000
/-
B2sum.Logic — Pure BLAKE2b hash implementation. 0BSD
-/

namespace Lentils.B2sum.Logic

open ByteArray

def arrGet (arr : Array UInt64) (i : Nat) : UInt64 :=
  if h : i < arr.size then arr[i] else 0

def byteGet (arr : ByteArray) (i : Nat) : UInt8 :=
  if h : i < arr.size then arr[i] else 0

def rotr (x : UInt64) (n : UInt64) : UInt64 :=
  (x >>> n) ||| (x <<< (64 - n))

def IV : Array UInt64 := #[
  0x6a09e667f3bcc908, 0xbb67ae8584caa73b, 0x3c6ef372fe94f82b, 0xa54ff53a5f1d36f1,
  0x510e527fade682d1, 0x9b05688c2b3e6c1f, 0x1f83d9abfb41bd6b, 0x5be0cd19137e2179]

def SIGMA_DATA : Array UInt64 :=
  #[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
    14,10,4,8,9,15,13,6,1,12,0,2,11,7,5,3,
    11,8,12,0,5,2,15,13,10,14,3,6,7,1,9,4,
    7,9,3,1,13,12,11,14,2,6,5,10,4,0,15,8,
    9,0,5,7,2,4,10,15,14,1,11,12,6,8,3,13,
    2,12,6,10,0,11,8,3,4,13,7,5,15,14,1,9,
    12,5,1,15,14,13,4,10,0,7,6,3,9,2,8,11,
    13,11,7,14,12,1,3,9,5,0,15,4,8,6,2,10,
    6,15,14,9,11,3,0,8,12,2,13,7,1,4,10,5,
    10,2,8,4,7,6,1,5,15,11,9,14,3,12,13,0]

def makeParam (outLen : UInt64) : Array UInt64 :=
  let p0 : UInt64 := outLen ||| ((0 : UInt64) <<< 8) ||| ((1 : UInt64) <<< 16) ||| ((1 : UInt64) <<< 24)
  #[p0, 0, 0, 0, 0, 0, 0, 0]

def initState (outLen : UInt64) : Array UInt64 :=
  let p := makeParam outLen
  List.toArray ((List.range 8).map (λ i => arrGet IV i ^^^ arrGet p i))

def G (v : Array UInt64) (a b c d : Nat) (x y : UInt64) : Array UInt64 :=
  let va := arrGet v a + arrGet v b + x
  let vd := rotr (arrGet v d ^^^ va) 32
  let vc := arrGet v c + vd
  let vb := rotr (arrGet v b ^^^ vc) 24
  let va2 := va + vb + y
  let vd2 := rotr (vd ^^^ va2) 16
  let vc2 := vc + vd2
  let vb2 := rotr (vb ^^^ vc2) 63
  let v1 := v.set! a va2
  let v2 := v1.set! b vb2
  let v3 := v2.set! c vc2
  v3.set! d vd2

def roundFunc (v : Array UInt64) (m : Array UInt64) (rnd : Nat) : Array UInt64 :=
  let base := (rnd % 10) * 16
  let s0 := (arrGet SIGMA_DATA base).toNat
  let s1 := (arrGet SIGMA_DATA (base+1)).toNat
  let s2 := (arrGet SIGMA_DATA (base+2)).toNat
  let s3 := (arrGet SIGMA_DATA (base+3)).toNat
  let s4 := (arrGet SIGMA_DATA (base+4)).toNat
  let s5 := (arrGet SIGMA_DATA (base+5)).toNat
  let s6 := (arrGet SIGMA_DATA (base+6)).toNat
  let s7 := (arrGet SIGMA_DATA (base+7)).toNat
  let s8 := (arrGet SIGMA_DATA (base+8)).toNat
  let s9 := (arrGet SIGMA_DATA (base+9)).toNat
  let s10 := (arrGet SIGMA_DATA (base+10)).toNat
  let s11 := (arrGet SIGMA_DATA (base+11)).toNat
  let s12 := (arrGet SIGMA_DATA (base+12)).toNat
  let s13 := (arrGet SIGMA_DATA (base+13)).toNat
  let s14 := (arrGet SIGMA_DATA (base+14)).toNat
  let s15 := (arrGet SIGMA_DATA (base+15)).toNat
  let g1 := G v 0 4 8 12 (arrGet m s0) (arrGet m s1)
  let g2 := G g1 1 5 9 13 (arrGet m s2) (arrGet m s3)
  let g3 := G g2 2 6 10 14 (arrGet m s4) (arrGet m s5)
  let g4 := G g3 3 7 11 15 (arrGet m s6) (arrGet m s7)
  let g5 := G g4 0 5 10 15 (arrGet m s8) (arrGet m s9)
  let g6 := G g5 1 6 11 12 (arrGet m s10) (arrGet m s11)
  let g7 := G g6 2 7 8 13 (arrGet m s12) (arrGet m s13)
  G g7 3 4 9 14 (arrGet m s14) (arrGet m s15)

def blake2bPad (data : ByteArray) : ByteArray :=
  if data.size == 0 then
    -- Empty input: one block of all zeros
    ByteArray.mk (List.toArray (List.replicate 128 0))
  else
    let remainder := data.size % 128
    if remainder == 0 then
      data  -- Already a full block, no padding needed
    else
      let padZeros := 128 - remainder
      let zeros : Array UInt8 := List.toArray (List.replicate padZeros 0)
      data ++ ByteArray.mk zeros

def readLE64 (block : ByteArray) (offset : Nat) : UInt64 :=
  let b0 := byteGet block offset
  let b1 := byteGet block (offset+1)
  let b2 := byteGet block (offset+2)
  let b3 := byteGet block (offset+3)
  let b4 := byteGet block (offset+4)
  let b5 := byteGet block (offset+5)
  let b6 := byteGet block (offset+6)
  let b7 := byteGet block (offset+7)
  (UInt64.ofNat b0.toNat) |||
  ((UInt64.ofNat b1.toNat) <<< 8) |||
  ((UInt64.ofNat b2.toNat) <<< 16) |||
  ((UInt64.ofNat b3.toNat) <<< 24) |||
  ((UInt64.ofNat b4.toNat) <<< 32) |||
  ((UInt64.ofNat b5.toNat) <<< 40) |||
  ((UInt64.ofNat b6.toNat) <<< 48) |||
  ((UInt64.ofNat b7.toNat) <<< 56)

def readWords (block : ByteArray) : Array UInt64 :=
  List.toArray (List.range 16 |>.map (λ i => readLE64 block (i * 8)))

def processBlock (state : Array UInt64) (block : ByteArray) (counter : UInt64) (isLast : Bool) : Array UInt64 :=
  let m := readWords block
  let v0 : Array UInt64 :=
    List.toArray ((List.range 8).map (λ i => arrGet state i) ++ (List.range 8).map (λ i => arrGet IV i))
  let v1 := if isLast then
    let old := arrGet v0 14
    v0.set! 14 (old ^^^ 0xFFFFFFFFFFFFFFFF)
  else v0
  let v2 := v1.set! 12 (arrGet v1 12 ^^^ counter)
  let v3 := v2.set! 13 (arrGet v2 13 ^^^ 0)
  let v4 := roundFunc v3 m 0
  let v5 := roundFunc v4 m 1
  let v6 := roundFunc v5 m 2
  let v7 := roundFunc v6 m 3
  let v8 := roundFunc v7 m 4
  let v9 := roundFunc v8 m 5
  let v10 := roundFunc v9 m 6
  let v11 := roundFunc v10 m 7
  let v12 := roundFunc v11 m 8
  let v13 := roundFunc v12 m 9
  let v14 := roundFunc v13 m 10
  let vFinal := roundFunc v14 m 11
  List.toArray ((List.range 8).map (λ i => arrGet state i ^^^ arrGet vFinal i ^^^ arrGet vFinal (i + 8)))

def blake2b (data : ByteArray) (outLen : UInt64 := 64) : ByteArray :=
  let outLen' := if outLen = 0 then 64 else outLen
  let state0 := initState outLen'
  let padded := blake2bPad data
  let numBlocks := padded.size / 128
  let origLen := UInt64.ofNat data.size
  let rec processAll (state : Array UInt64) (i : Nat) : Array UInt64 :=
    if i ≥ numBlocks then state
    else
      let block := padded.extract (i * 128) ((i + 1) * 128)
      let last := i + 1 = numBlocks
      let ctr := if last then origLen else UInt64.ofNat ((i + 1) * 128)
      processAll (processBlock state block ctr last) (i + 1)
  let finalState := processAll state0 0
  let encodeWord (w : UInt64) : List UInt8 :=
    [((w >>> 0).land 0xFF).toUInt8,
     ((w >>> 8).land 0xFF).toUInt8,
     ((w >>> 16).land 0xFF).toUInt8,
     ((w >>> 24).land 0xFF).toUInt8,
     ((w >>> 32).land 0xFF).toUInt8,
     ((w >>> 40).land 0xFF).toUInt8,
     ((w >>> 48).land 0xFF).toUInt8,
     ((w >>> 56).land 0xFF).toUInt8]
  ByteArray.mk (List.toArray (
    (encodeWord (arrGet finalState 0) ++
     encodeWord (arrGet finalState 1) ++
     encodeWord (arrGet finalState 2) ++
     encodeWord (arrGet finalState 3) ++
     encodeWord (arrGet finalState 4) ++
     encodeWord (arrGet finalState 5) ++
     encodeWord (arrGet finalState 6) ++
     encodeWord (arrGet finalState 7)).take outLen'.toNat
  ))

def blake2b512 (data : ByteArray) : ByteArray := blake2b data 64

def formatHex (hash : ByteArray) : String :=
  String.ofList (List.flatten (hash.toList.map (λ b =>
    let hi := b.shiftRight 4
    let lo := b.land 0x0F
    [Char.ofNat (if hi < 10 then 0x30 + hi.toNat else 0x57 + hi.toNat),
     Char.ofNat (if lo < 10 then 0x30 + lo.toNat else 0x57 + lo.toNat)]
  )))

def formatStdin (data : ByteArray) : String :=
  formatHex (blake2b512 data) ++ "  -\n"

-- native_decide on blake2b is too heavy for the kernel; runtime verification via tests.

-- ─── Intermediate Function Proofs ─────────────────────────────────────────────
-- These proofs are kept simple to avoid kernel timeout on heavy computations

-- SIGMA permutation table properties
example : SIGMA_DATA.size = 160 := rfl  -- 10 rounds × 16 entries

-- Rotation helper proofs
example : rotr (0x0000000000000001 : UInt64) 1 = (0x8000000000000000 : UInt64) := by native_decide
example : rotr (0x8000000000000000 : UInt64) 1 = (0x4000000000000000 : UInt64) := by native_decide
example : rotr (0xFFFFFFFFFFFFFFFF : UInt64) 64 = (0xFFFFFFFFFFFFFFFF : UInt64) := by native_decide

-- Initialization vector proofs
example : IV.size = 8 := rfl

-- Initial state with default output length
example : (initState 64).size = 8 := by native_decide

-- Padding proofs
example : (blake2bPad ByteArray.empty).size = 128 := by native_decide
example : (blake2bPad (ByteArray.mk (List.toArray (List.replicate 128 0x41)))).size = 128 := by native_decide

-- Format hex proof
example : formatHex ByteArray.empty = "" := by native_decide

end Lentils.B2sum.Logic
