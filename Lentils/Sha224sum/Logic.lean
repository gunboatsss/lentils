/-
Sha224sum.Logic — Verified pure SHA-224 hash implementation. 0BSD
Reuses SHA-256 core with SHA-224 initial values and truncated output (28 bytes).
Provenance: FIPS 180-4.
-/

import Lentils.Sha256sum.Logic

namespace Lentils.Sha224sum.Logic
open Lentils.Sha256sum.Logic

structure HashInput where
  data : ByteArray
  deriving Inhabited, BEq

def sha224 (data : ByteArray) : ByteArray := sha256WithInit initH224 7 data
def formatHex (hash : ByteArray) : String := Lentils.Sha256sum.Logic.formatHex hash
def formatStdin (data : ByteArray) : String := formatHex (sha224 data) ++ "  -\n"

def spec (input : HashInput) : ByteArray := sha224 input.data

theorem i_output_size (data : ByteArray) : (sha224 data).size = 28 := by
  have h := sha256WithInit_size initH224 7 data
  simpa [sha224] using h

theorem i_formatHex_empty : formatHex ByteArray.empty = "" := Lentils.Sha256sum.Logic.i_formatHex_empty
theorem i_formatStdin_compose (data : ByteArray) : formatStdin data = formatHex (sha224 data) ++ "  -\n" := rfl

example : sha224 ByteArray.empty = ByteArray.mk (List.toArray ([0xd1,0x4a,0x02,0x8c,0x2a,0x3a,0x2b,0xc9,0x47,0x61,0x02,0xbb,0x28,0x82,0x34,0xc4,0x15,0xa2,0xb0,0x1f,0x82,0x8e,0xa6,0x2a,0xc5,0xb3,0xe4,0x2f] : List UInt8)) := by native_decide
example : sha224 "abc".toUTF8 = ByteArray.mk (List.toArray ([0x23,0x09,0x7d,0x22,0x34,0x05,0xd8,0x22,0x86,0x42,0xa4,0x77,0xbd,0xa2,0x55,0xb3,0x2a,0xad,0xbc,0xe4,0xbd,0xa0,0xb3,0xf7,0xe3,0x6c,0x9d,0xa7] : List UInt8)) := by native_decide
example : (sha224 ByteArray.empty).size = 28 := i_output_size ByteArray.empty

end Lentils.Sha224sum.Logic
