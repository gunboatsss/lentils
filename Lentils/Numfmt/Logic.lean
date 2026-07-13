/-
Numfmt.Logic — Pure number-formatting logic for `numfmt`. 0BSD

Converts numbers to and from human-readable scaled forms
(SI = powers of 1000, IEC = powers of 1024). Pure; proofs use
native_decide.
-/

namespace Lentils.Numfmt.Logic

/-- Total list indexing with a fallback (List.get! is unavailable). -/
def listGet {α} (l : List α) (i : Nat) (d : α) : α :=
  let rec go (xs : List α) (j : Nat) : α :=
    match xs with
    | [] => d
    | x :: xs => if j = 0 then x else go xs (j - 1)
  go l i

inductive Mode | passthrough | toSI | toIEC | fromSI | fromIEC
  deriving Inhabited, DecidableEq

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

def findDigitPos (chars : List Char) : Nat :=
  let rec go (cs : List Char) (i : Nat) : Nat :=
    match cs with
    | [] => chars.length
    | c :: rest => if c.isDigit then i else go rest (i + 1)
  go chars 0

def scanEnd (chars : List Char) (start : Nat) : Nat :=
  let rec go (j : Nat) (seenDot : Bool) : Nat :=
    if j ≥ chars.length then j
    else
      let c := listGet chars j ' '
      if c.isDigit then go (j + 1) seenDot
      else if c == '.' && !seenDot then go (j + 1) true
      else if c.isAlpha then go (j + 1) seenDot
      else j
  go start false

def computeStart (chars : List Char) (di : Nat) : Nat :=
  if di > 0 && (listGet chars (di - 1) ' ' == '-' || listGet chars (di - 1) ' ' == '+') then di - 1 else di

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

-- ─── Proofs ──────────────────────────────────────────────────────────────────

example : letterExp 'K' = some 1 := by native_decide
example : expToLetter 3 = "G" := by native_decide
example : convertNum "1500" Mode.toSI none = "1.5K" := by native_decide
example : convertNum "1000" Mode.toSI none = "1.0K" := by native_decide
example : convertNum "1K" Mode.fromSI none = "1000" := by native_decide
example : convertNum "1024" Mode.toIEC none = "1.0Ki" := by native_decide
example : numfmt "1500" Mode.toSI none = "1.5K" := by native_decide

end Lentils.Numfmt.Logic
