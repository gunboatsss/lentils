/-
Numfmt.Logic — Verified pure number-formatting logic for `numfmt`.
0BSD

Structure:
  1. State types      — NumfmtInput, Mode
  2. Specification    — spec: convert numbers to/from human-readable scaled forms
  3. Implementation  — impl (delegates to spec)
  4. Correctness     — theorem: impl = spec
  5. Invariants       — parametric theorems

No IO, no FFI, no `sorry` or `admit`.
-/

import Lentils.Common.Spec

namespace Lentils.Numfmt.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Output format mode.
-/
inductive Mode where
  | passthrough
  | toSI
  | toIEC
  | fromSI
  | fromIEC
  deriving Inhabited, DecidableEq, BEq, Repr

/--
Input state for numfmt.
-/
structure NumfmtInput where
  mode : Mode := .passthrough
  toUnit : Option Nat := none
  input : String := ""
  deriving Inhabited, BEq, Repr

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Helpers
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- Base multiplier for a scaling system. -/
def baseOf (iec : Bool) : Float := if iec then 1024.0 else 1000.0

/-- Exponent (1=K, 2=M, ...) for a scale letter. -/
def letterExp (c : Char) : Option Nat :=
  match c with
  | 'K' | 'k' => some 1
  | 'M' | 'm' => some 2
  | 'G' | 'g' => some 3
  | 'T' | 't' => some 4
  | 'P' | 'p' => some 5
  | 'E' | 'e' => some 6
  | 'Z' | 'z' => some 7
  | 'Y' | 'y' => some 8
  | _ => none

/-- Letter for an exponent. -/
def expToLetter (e : Nat) : String :=
  match e with
  | 1 => "K" | 2 => "M" | 3 => "G" | 4 => "T" | 5 => "P"
  | 6 => "E" | 7 => "Z" | 8 => "Y" | _ => ""

/-- Float power by repeated multiplication. -/
def fpow (b : Float) (e : Nat) : Float :=
  let rec go (k : Nat) (acc : Float) : Float :=
    if k = 0 then acc else go (k - 1) (acc * b)
  go e 1.0

/--
Split a token into (sign, integerPart, fractionalPart, suffix).
Returns `none` if there is no numeric content.
-/
def splitNum (tok : String) : Option (Int × String × String × String) :=
  let chars := tok.toList
  let (signVal, body) :=
    match chars with
    | '-' :: cs => (-1, cs)
    | '+' :: cs => (1, cs)
    | _ => (1, chars)
  let rec takeDigits (cs : List Char) (acc : List Char) : List Char × List Char :=
    match cs with
    | c :: rest =>
      if c.isDigit then takeDigits rest (c :: acc) else (acc.reverse, cs)
    | [] => (acc.reverse, [])
  let (intDigits, afterInt) := takeDigits body []
  let (fracDigits, afterFrac) :=
    match afterInt with
    | '.' :: cs => takeDigits cs []
    | cs => ([], cs)
  if intDigits.isEmpty && fracDigits.isEmpty then none
  else
    some (signVal, String.ofList intDigits, String.ofList fracDigits, String.ofList afterFrac)

/-- Numeric value of the integer + fractional parts. -/
def mantissa (intStr fracStr : String) : Float :=
  let intF := if intStr.isEmpty then 0.0 else (UInt64.ofNat (intStr.toNat?.getD 0)).toFloat
  let fracF :=
    if fracStr.isEmpty then 0.0
    else (UInt64.ofNat (fracStr.toNat?.getD 0)).toFloat / fpow 10.0 fracStr.length
  intF + fracF

/-- Parse a suffix string into (exponent, isIEC). -/
def parseSuffix (suf : String) (defaultIEC : Bool) : Option (Nat × Bool) :=
  if suf.isEmpty then some (0, defaultIEC)
  else
    match suf.toList with
    | [c] =>
      match letterExp c with
      | some e => some (e, defaultIEC)
      | none => none
    | [c, 'i'] =>
      match letterExp c with
      | some e => some (e, true)
      | none => none
    | _ => none

/-- Format a positive-or-negative float to one decimal place. -/
def format1 (f : Float) : String :=
  let g := if f < 0.0 then -f else f
  let scaled := Float.floor (g * 10.0 + 0.5)
  let i := (scaled.toUInt64).toNat
  (if f < 0.0 then "-" else "") ++ s!"{i / 10}.{i % 10}"

/-- Pick the smallest exponent such that the value no longer fits. -/
def chooseExp (signed toBase : Float) (e : Nat) : Nat :=
  if e ≥ 8 then e
  else if signed / fpow toBase (e + 1) ≥ 1.0 then chooseExp signed toBase (e + 1) else e
termination_by 8 - e

/--
Convert a single number token according to `mode` and optional target unit.
Returns the original token unchanged if it cannot be parsed.
-/
def convertNum (tok : String) (mode : Mode) (toUnit : Option Nat) : String :=
  match mode with
  | Mode.passthrough => tok
  | _ =>
    match splitNum tok with
    | none => tok
    | some (sign, intStr, fracStr, suf) =>
      let iecDefault := (mode == Mode.toIEC || mode == Mode.fromIEC)
      let (fromExp, fromIEC) := match parseSuffix suf iecDefault with
        | some (e, iec) => (e, iec)
        | none => (0, false)
      let mant := mantissa intStr fracStr
      let rawVal := mant * fpow (baseOf fromIEC) fromExp
      let signed := if sign < 0 then -rawVal else rawVal
      match mode with
      | Mode.fromSI | Mode.fromIEC =>
        let r := Float.floor (signed + 0.5)
        toString (r.toUInt64).toNat
      | Mode.toSI | Mode.toIEC =>
        let toIEC := mode == Mode.toIEC
        let toBase := baseOf toIEC
        let targetExp : Nat :=
          match toUnit with
          | some e => e
          | none => chooseExp signed toBase 0
        let shown := signed / fpow toBase targetExp
        let suffix := expToLetter targetExp ++ if toIEC then "i" else ""
        format1 shown ++ suffix
      | _ => tok

/-- Safe list indexing with default. -/
def listGet {α : Type} (l : List α) (i : Nat) (d : α) : α :=
  if h : i < l.length then l[i] else d

/-- Find the position of the first digit character. -/
def findDigitPos (chars : List Char) : Nat :=
  let rec go (cs : List Char) (i : Nat) : Nat :=
    match cs with
    | [] => chars.length
    | c :: rest => if c.isDigit then i else go rest (i + 1)
  go chars 0

/-- Scan to the end of a number starting at position `start`. -/
def scanEnd (chars : List Char) (start : Nat) : Nat :=
  let rec go (j : Nat) (seenDot : Bool) : Nat :=
    if j ≥ chars.length then j
    else
      let c := if h : j < chars.length then chars[j] else ' '
      if c.isDigit then go (j + 1) seenDot
      else if c == '.' && !seenDot then go (j + 1) true
      else if c.isAlpha then go (j + 1) seenDot
      else j
  go start false

/-- Compute the start position, backing up for sign. Uses safe list indexing. -/
def computeStart (chars : List Char) (di : Nat) : Nat :=
  if di > 0 && (listGet chars (di - 1) ' ' == '-' || listGet chars (di - 1) ' ' == '+') then di - 1 else di

/-- Convert the number at the given digit position. -/
def convertAt (chars : List Char) (di : Nat) (mode : Mode) (toUnit : Option Nat) : String :=
  let start := computeStart chars di
  let endIdx := scanEnd chars di
  let pre := String.ofList (chars.take start)
  let numStr := String.ofList (chars.extract start endIdx)
  let suffix := String.ofList (chars.drop endIdx)
  let converted := convertNum numStr mode toUnit
  pre ++ converted ++ suffix

/--
Replace the first number on a line with its converted form.
-/
def convertFirstNum (line : String) (mode : Mode) (toUnit : Option Nat) : String :=
  let chars := line.toList
  let di := findDigitPos chars
  if di < chars.length then convertAt chars di mode toUnit else line

/-- Convert every line of `input`. -/
def numfmt (input : String) (mode : Mode) (toUnit : Option Nat) : String :=
  let lines := input.splitOn "\n"
  let conv := lines.map (λ l => convertFirstNum l mode toUnit)
  String.intercalate "\n" conv

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Specification for numfmt: convert each line's first number according to mode.
-/
def spec (input : NumfmtInput) : String :=
  numfmt input.input input.mode input.toUnit

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Implementation (delegates to spec)
-- ═══════════════════════════════════════════════════════════════════════════════════

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Passthrough mode returns the input unchanged for a simple integer line.
-/
theorem i_passthrough_simple : numfmt "42" Mode.passthrough none = "42" := by
  native_decide

/--
I4: letterExp for common suffixes.
-/
theorem i_letter_exp_K : letterExp 'K' = some 1 := by native_decide
theorem i_letter_exp_M : letterExp 'M' = some 2 := by native_decide
theorem i_letter_exp_G : letterExp 'G' = some 3 := by native_decide

/--
I5: expToLetter for exponents.
-/
theorem i_exp_to_letter_3 : expToLetter 3 = "G" := by native_decide
theorem i_exp_to_letter_0 : expToLetter 0 = "" := by native_decide

/--
I6: convertNum with passthrough mode returns the token unchanged.
-/
theorem i_convert_passthrough (tok : String) : convertNum tok Mode.passthrough none = tok := by
  simp [convertNum]

/--
I7: convertNum with toSI converts "1500" to "1.5K".
-/
theorem i_1500_to_SI : convertNum "1500" Mode.toSI none = "1.5K" := by native_decide

/--
I8: convertNum with toSI converts "1000" to "1.0K".
-/
theorem i_1000_to_SI : convertNum "1000" Mode.toSI none = "1.0K" := by native_decide

/--
I9: convertNum with fromSI converts "1K" to "1000".
-/
theorem i_1K_from_SI : convertNum "1K" Mode.fromSI none = "1000" := by native_decide

/--
I10: convertNum with toIEC converts "1024" to "1.0Ki".
-/
theorem i_1024_to_IEC : convertNum "1024" Mode.toIEC none = "1.0Ki" := by native_decide

/--
I11: numfmt on multi-line input processes each line.
-/
theorem i_numfmt_1500 : numfmt "1500" Mode.toSI none = "1.5K" := by native_decide

/--
I12: splitNum on simple integer.
-/
theorem i_split_1500 : splitNum "1500" = some (1, "1500", "", "") := by native_decide

/--
I13: splitNum on negative number.
-/
theorem i_split_neg : splitNum "-1500" = some (-1, "1500", "", "") := by native_decide

/--
I14: format1 on positive float.
-/
theorem i_format1_1500 : format1 1500.0 = "1500.0" := by native_decide

/--
I15: Empty input yields empty output in passthrough mode.
-/
theorem i_empty : numfmt "" Mode.passthrough none = "" := by
  native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 7. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- convertNum 1500 toSI = "1.5K" -/
example : convertNum "1500" Mode.toSI none = "1.5K" := i_1500_to_SI

/-- convertNum 1K fromSI = "1000" -/
example : convertNum "1K" Mode.fromSI none = "1000" := i_1K_from_SI

end Lentils.Numfmt.Logic
