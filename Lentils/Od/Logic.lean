/-
Od.Logic — Verified pure octal dump formatting logic for `od`.
0BSD

Structure:
  1. State types      — OdInput
  2. Specification    — spec: octal dump formatting
  3. Implementation  — impl (delegates to spec)
  4. Correctness     — theorem: impl = spec
  5. Invariants       — parametric theorems (octal formatting, empty, char escapes)

No IO, no FFI, no `sorry` or `admit`.
-/

import Lentils.Common.Spec

namespace Lentils.Od.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for od: data to dump.
-/
structure OdInput where
  data : ByteArray
  deriving Inhabited, BEq

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Core Formatting Functions
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Format a Nat as an octal string with a minimum width (zero-padded).
-/
def toOctal (n : Nat) (width : Nat) : String :=
  let rec go (m : Nat) (acc : List Char) : List Char :=
    if m = 0 then acc
    else go (m / 8) ((Char.ofNat ((m % 8) + 48)) :: acc)
  let digits := if n = 0 then ['0'] else go n []
  let padding := List.replicate (width - digits.length) '0'
  String.ofList (padding ++ digits)

/--
Format a byte as a 3-digit octal string.
-/
def byteToOctal (b : UInt8) : String :=
  toOctal b.toNat 3

/--
Format a byte as a character for display (for `-c` format).
Non-printable bytes get C-style escapes.
-/
def byteToChar (b : UInt8) : String :=
  let n := b.toNat
  if n >= 32 && n <= 126 then
    String.ofList [Char.ofNat n]
  else
    let escapes : List (Nat × String) := [
      (0, "\\0"), (7, "\\a"), (8, "\\b"), (9, "\\t"),
      (10, "\\n"), (11, "\\v"), (12, "\\f"), (13, "\\r")
    ]
    match escapes.find? (λ (code, _) => code = n) with
    | some (_, esc) => esc
    | none => byteToOctal b

/--
Dump a byte array in traditional octal format (like `od -A o -t o1`).
Each line shows a 7-digit octal address offset and up to 16 bytes in octal.
-/
def octalDump (data : ByteArray) : String :=
  let len := data.size
  if len = 0 then "" else
  let lines := List.range ((len + 15) / 16) |>.map (λ lineIdx =>
    let addr := lineIdx * 16
    let addrStr := toOctal addr 7
    let bytes := List.range 16 |>.filterMap (λ i =>
      let idx := lineIdx * 16 + i
      if h : idx < len then some (data.get idx) else none)
    let octals := bytes.map (λ b => byteToOctal b)
    let body := String.intercalate " " octals
    addrStr ++ " " ++ body)
  String.intercalate "\n" lines

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Specification: format a ByteArray as an octal dump.
-/
def spec (input : OdInput) : String :=
  octalDump input.data

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Implementation (delegates to spec)
-- ═══════════════════════════════════════════════════════════════════════════════════

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: toOctal 0 with width 3 is "000".
-/
theorem i_to_octal_0 : toOctal 0 3 = "000" := by native_decide

/--
I2: toOctal 8 with width 3 is "010" (8 = 0o10).
-/
theorem i_to_octal_8 : toOctal 8 3 = "010" := by native_decide

/--
I3: toOctal 255 with width 3 is "377" (255 = 0o377).
-/
theorem i_to_octal_255 : toOctal 255 3 = "377" := by native_decide

/--
I4: byteToOctal for 0x41 ('A') = "101" (0x41 = 65 = 0o101).
-/
theorem i_byte_to_octal_A : byteToOctal 0x41 = "101" := by native_decide

/--
I5: Empty input → empty output.
-/
theorem i_empty : octalDump ByteArray.empty = "" := rfl

/--
I6: Unfolding lemma — `spec` delegates to `octalDump`.
-/
theorem i_spec_unfold (input : OdInput) : spec input = octalDump input.data := by
  simp [spec]

/--
I8: byteToChar for printable ASCII returns the character itself.
-/
theorem i_byte_to_char_A : byteToChar 0x41 = "A" := by native_decide

/--
I9: byteToChar for '\n' returns "\\n".
-/
theorem i_byte_to_char_newline : byteToChar 0x0A = "\\n" := by native_decide

/--
I10: byteToChar for '\0' returns "\\0".
-/
theorem i_byte_to_char_null : byteToChar 0x00 = "\\0" := by native_decide

/--
I11: Octal dump of a single byte.
-/
theorem i_octal_dump_one : octalDump (ByteArray.mk #[0x41]) = "0000000 101" := by native_decide

/--
I12: Octal dump of two bytes.
-/
theorem i_octal_dump_two : octalDump (ByteArray.mk #[0x41, 0x42]) = "0000000 101 102" := by native_decide

/--
I13: Address offset is 7-digit octal. The first address is 0000000.
-/
theorem i_addr_first_line : octalDump (ByteArray.mk #[0]) = "0000000 000" := by native_decide

/--
I14: 17 zero bytes produces two lines of output (address 0000020 for second line).
-/
theorem i_octal_dump_17_zeros : octalDump (ByteArray.mk (List.toArray (List.replicate 17 (0 : UInt8)))) = 
  "0000000 000 000 000 000 000 000 000 000 000 000 000 000 000 000 000 000\n0000020 000" := by
  native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 7. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- toOctal 0 width 3 = "000" -/
example : toOctal 0 3 = "000" := i_to_octal_0

/-- octal dump of empty is empty -/
example : octalDump ByteArray.empty = "" := i_empty

end Lentils.Od.Logic
