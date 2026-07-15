/-
Basenc.Logic — Pure logic for the `basenc` utility.
0BSD

Supports base64, base32, and base16 encoding/decodeing by delegating to
the existing implementation modules.

Provenance: GNU coreutils `basenc`.
No GPL source was consulted.
-/

namespace Lentils.Basenc.Logic

/--
Encoding mode selected by the user.
-/
inductive Encoding
  | base64
  | base32
  | base16
  deriving Repr, BEq, DecidableEq

/--
Parsed options for `basenc`.
-/
structure Options where
  encoding : Encoding := .base64
  decode : Bool := false
  wrap : Nat := 76
  deriving Repr, BEq, DecidableEq

/--
Parse `basenc` arguments into `(options, files)`.

  basenc [--base64 | --base32 | --base16] [-d] [FILE...]
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

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- Parse --base32 flag. -/
theorem parse_base32 :
  (parseArgs ["--base32"]).1.encoding = .base32 := by native_decide

/-- Parse -d flag. -/
theorem parse_decode :
  (parseArgs ["-d"]).1.decode = true := by native_decide

/-- Empty base16 encode. -/
theorem encode_base16_empty : encodeBase16 ByteArray.empty = "" := by native_decide

/-- Base16 encode "abc". -/
theorem encode_base16_abc :
  encodeBase16 "abc".toUTF8 = "616263" := by native_decide

/-- Base16 decode "616263". -/
theorem decode_base16_616263 :
  decodeBase16 "616263" = some "abc".toUTF8 := by native_decide

end Lentils.Basenc.Logic
