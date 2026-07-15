/-
Sha224sum.Logic — Pure logic for the `sha224sum` utility.
0BSD

Reuses the SHA-256 core from Sha256sum.Logic with SHA-224 initial values
and truncated output (28 bytes instead of 32).

Provenance: FIPS 180-4 (SHA-224).
No GPL source was consulted.
-/

import Lentils.Sha256sum.Logic

namespace Lentils.Sha224sum.Logic

open Lentils.Sha256sum.Logic

/--
Compute the SHA-224 hash of `data`.

SHA-224 uses the same algorithm as SHA-256 but with different initial
hash values and output truncated to 7 words (28 bytes).
-/
def sha224 (data : ByteArray) : ByteArray :=
  sha256WithInit initH224 7 data

/-- Format a hash as a lowercase hex string. -/
def formatHex (hash : ByteArray) : String :=
  Lentils.Sha256sum.Logic.formatHex hash

/-- Format stdin input as \"<hash>  -\\n\". -/
def formatStdin (data : ByteArray) : String :=
  formatHex (sha224 data) ++ "  -\n"

-- ─── Proofs ──────────────────────────────────────────────────────────────────

-- SHA-224 empty string test vector
example : sha224 ByteArray.empty = ByteArray.mk (List.toArray
  ([0xd1, 0x4a, 0x02, 0x8c, 0x2a, 0x3a, 0x2b, 0xc9,
    0x47, 0x61, 0x02, 0xbb, 0x28, 0x82, 0x34, 0xc4,
    0x15, 0xa2, 0xb0, 0x1f, 0x82, 0x8e, 0xa6, 0x2a,
    0xc5, 0xb3, 0xe4, 0x2f] : List UInt8)) := by native_decide

-- SHA-224 "abc" test vector
example : sha224 "abc".toUTF8 = ByteArray.mk (List.toArray
  ([0x23, 0x09, 0x7d, 0x22, 0x34, 0x05, 0xd8, 0x22,
    0x86, 0x42, 0xa4, 0x77, 0xbd, 0xa2, 0x55, 0xb3,
    0x2a, 0xad, 0xbc, 0xe4, 0xbd, 0xa0, 0xb3, 0xf7,
    0xe3, 0x6c, 0x9d, 0xa7] : List UInt8)) := by native_decide

end Lentils.Sha224sum.Logic
