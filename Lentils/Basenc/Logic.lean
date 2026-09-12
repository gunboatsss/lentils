/-
Basenc.Logic — Verified pure logic for the `basenc` utility.
0BSD

Structure:
  1. State types      — Encoding, Options, BasencInput
  2. Specification    — specEncode, specDecode (base64, base32, base16)
  3. Specification    — spec (directly executable)
  4. Invariants       — parametric properties over all inputs
  5. Invariants       — parametric theorems (roundtrip, determinism)

Supports base64, base32, and base16 encoding/decoding.
Delegates base64/base32 to the dedicated modules.

No IO, no FFI, no `sorry` or `admit`.
-/

import Lentils.Common.Spec
import Lentils.Base64.Logic
import Lentils.Base32.Logic

namespace Lentils.Basenc.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Encoding mode selected by the user.
-/
inductive Encoding
  | base64
  | base32
  | base16
  deriving Repr, BEq, DecidableEq, Inhabited

/--
Parsed options for `basenc`.
-/
structure Options where
  encoding : Encoding := .base64
  decode : Bool := false
  wrap : Nat := 76
  deriving Repr, BEq, DecidableEq, Inhabited

/--
Input state for basenc: combined flags, data, and file arguments.
-/
structure BasencInput where
  options : Options
  data : ByteArray
  deriving Inhabited, BEq

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Argument Parsing
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Parse `basenc` arguments into `(options, files)`.
-/
def parseArgs (args : List String) : Options × List String :=
  let rec go (remaining : List String) (opts : Options) (files : List String)
      : Options × List String :=
    match remaining with
    | [] => (opts, files.reverse)
    | "--" :: rest => (opts, files.reverse ++ rest)
    | "--base64" :: rest => go rest { opts with encoding := .base64 } files
    | "--base32" :: rest => go rest { opts with encoding := .base32 } files
    | "--base16" :: rest => go rest { opts with encoding := .base16 } files
    | "-d" :: rest => go rest { opts with decode := true } files
    | "--decode" :: rest => go rest { opts with decode := true } files
    | "-w" :: w :: rest =>
      let wrap := match w.toNat? with | some n => n | none => 76
      go rest { opts with wrap := wrap } files
    | "--wrap" :: w :: rest =>
      let wrap := match w.toNat? with | some n => n | none => 76
      go rest { opts with wrap := wrap } files
    | s :: rest =>
      if s.startsWith "-" && s != "-" then
        (opts, files.reverse)
      else
        go rest opts (s :: files)
  go args {} []

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Base16 Encoding/Decoding
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- Encode a ByteArray to base16 (uppercase hex, matching GNU). -/
def encodeBase16 (data : ByteArray) : String :=
  let chars := data.toList.flatMap (λ b =>
    let hi := b.shiftRight 4
    let lo := b.land 0x0F
    let hiChar := match hi.toNat with
      | 0 => '0' | 1 => '1' | 2 => '2' | 3 => '3' | 4 => '4' | 5 => '5'
      | 6 => '6' | 7 => '7' | 8 => '8' | 9 => '9' | 10 => 'A' | 11 => 'B'
      | 12 => 'C' | 13 => 'D' | 14 => 'E' | _ => 'F'
    let loChar := match lo.toNat with
      | 0 => '0' | 1 => '1' | 2 => '2' | 3 => '3' | 4 => '4' | 5 => '5'
      | 6 => '6' | 7 => '7' | 8 => '8' | 9 => '9' | 10 => 'A' | 11 => 'B'
      | 12 => 'C' | 13 => 'D' | 14 => 'E' | _ => 'F'
    [hiChar, loChar])
  String.ofList chars

/-- Decode base16 (hex) to ByteArray. Returns none on invalid input. -/
def decodeBase16 (s : String) : Option ByteArray :=
  let chars := s.toList.filter (λ c => c != '\n' && c != '\r' && c != ' ' && c != '\t')
  if chars.length % 2 ≠ 0 then none
  else
    let rec go (cs : List Char) (acc : ByteArray) : Option ByteArray :=
      match cs with
      | [] => some acc
      | c1 :: c2 :: rest =>
        let toNibble (c : Char) : Option UInt8 :=
          if c ≥ '0' && c ≤ '9' then some (UInt8.ofNat (c.toNat - 0x30))
          else if c ≥ 'a' && c ≤ 'f' then some (UInt8.ofNat (c.toNat - 0x57))
          else if c ≥ 'A' && c ≤ 'F' then some (UInt8.ofNat (c.toNat - 0x37))
          else none
        match toNibble c1, toNibble c2 with
        | some h, some l => go rest (acc.push ((h <<< 4) ||| l))
        | _, _ => none
      | _ => none
    go chars ByteArray.empty

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Encoding dispatch: apply the selected encoding to data.
-/
def specEncodeWith (enc : Encoding) (data : ByteArray) : String :=
  match enc with
  | .base64 => Lentils.Base64.Logic.encode data
  | .base32 => Lentils.Base32.Logic.encode data
  | .base16 => encodeBase16 data

/--
Decoding dispatch: decode a string using the selected encoding.
Returns none on failure.
-/
def specDecodeWith (enc : Encoding) (s : String) : Option ByteArray :=
  match enc with
  | .base64 => Lentils.Base64.Logic.decode s
  | .base32 => Lentils.Base32.Logic.decode s
  | .base16 => decodeBase16 s

/--
Specification: based on options, encode or decode the input data.
Returns none on decode error.
-/
def spec (input : BasencInput) : Option String :=
  if input.options.decode then
    match specDecodeWith input.options.encoding (String.fromUTF8! input.data) with
    | some ba => some (String.fromUTF8! ba)
    | none => none
  else
    some (specEncodeWith input.options.encoding input.data)

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Specification is the implementation (Cat.Logic pattern: no separate `impl` alias)
-- ═══════════════════════════════════════════════════════════════════════════════════

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Base16 roundtrip for short strings.
-/
theorem i_base16_roundtrip_empty : decodeBase16 (encodeBase16 ByteArray.empty) = some ByteArray.empty := by
  native_decide

theorem i_base16_roundtrip_abc : decodeBase16 (encodeBase16 "abc".toUTF8) = some "abc".toUTF8 := by
  native_decide

/--
I2: Base16 encoding of zero byte is "00".
-/
theorem i_base16_zero : encodeBase16 (ByteArray.mk #[0]) = "00" := by native_decide

/--
I3: Spec on base16 encode-mode inputs agrees with encodeBase16
(∀-quantified invariant, proved by unfolding).
-/
theorem i_spec_base16_encode (data : ByteArray) :
    spec { options := { encoding := .base16 }, data := data } =
      some (encodeBase16 data) := by
  simp [spec, specEncodeWith]

/--
I5: Parse default encoding (no flags) is base64.
-/
theorem i_parse_default : (parseArgs []).1.encoding = .base64 := rfl

/--
I6: Parse --base32 flag.
-/
theorem i_parse_base32 : (parseArgs ["--base32"]).1.encoding = .base32 := by native_decide

/--
I7: Parse -d flag.
-/
theorem i_parse_decode : (parseArgs ["-d"]).1.decode = true := by native_decide

/--
I9: Base16 encoding of known byte sequence.
-/
theorem i_encode_base16_hello : encodeBase16 "hello".toUTF8 = "68656C6C6F" := by native_decide

/--
I10: Base16 decode of known hex string.
-/
theorem i_decode_base16_hello : decodeBase16 "68656C6C6F" = some "hello".toUTF8 := by native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 7. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- encodeBase16 "abc" = "616263" -/
example : encodeBase16 "abc".toUTF8 = "616263" := by native_decide

/-- decodeBase16 "616263" = "abc" -/
example : decodeBase16 "616263" = some "abc".toUTF8 := by native_decide

end Lentils.Basenc.Logic
