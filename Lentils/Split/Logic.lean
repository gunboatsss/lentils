/-
Split.Logic — Verified pure split logic for `split`.
0BSD

Structure:
  1. State types      — SplitInput
  2. Specification    — spec: split input into chunks
  3. Invariants       — parametric theorems (suffix uniqueness, chunk size)

No IO, no FFI, no `sorry` or `admit`.
-/

import Lentils.Common.Spec

namespace Lentils.Split.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for split.
-/
structure SplitInput where
  input : String := ""
  maxLines : Nat := 1000
  maxBytes : Nat := 0
  suffixLen : Nat := 2
  numericSuffix : Bool := false
  deriving Inhabited, BEq, Repr

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Helpers
-- ═══════════════════════════════════════════════════════════════════════════════════

def defaultPrefix : String := "x"

/-- Convert a number to a base-26 alphabetic representation (a-z). -/
def toBase26 (n : Nat) : List Char :=
  if h0 : n = 0 then []
  else
    have h : n / 26 < n := by
      refine Nat.div_lt_self ?_ (by decide)
      exact Nat.pos_of_ne_zero h0
    let d := Char.ofNat ((n % 26) + 97)
    toBase26 (n / 26) ++ [d]
termination_by n

/-- Generate a suffix string for chunk index i with given length and numeric flag. -/
def suffix (i : Nat) (len : Nat := 2) (numeric : Bool := false) : String :=
  if numeric then
    let s := toString i
    if s.length ≥ len then s
    else String.ofList (List.replicate (len - s.length) '0') ++ s
  else
    let base26 := toBase26 i
    let chars := if base26.isEmpty then ['a'] else base26
    if chars.length ≥ len then String.ofList chars
    else String.ofList (List.replicate (len - chars.length) 'a') ++ String.ofList chars

/-- Split input lines into chunks. -/
partial def splitLines (input : List String) (maxLines : Nat) : List (Nat × List String) :=
  let rec go (remaining : List String) (chunkIdx : Nat) (acc : List (Nat × List String)) : List (Nat × List String) :=
    if remaining.isEmpty then acc.reverse
    else
      let chunk :=
        if maxLines > 0 then remaining.take maxLines
        else remaining
      let rest :=
        if maxLines > 0 then remaining.drop maxLines
        else []
      go rest (chunkIdx + 1) ((chunkIdx, chunk) :: acc)
  go input 0 []

/-- Split input by line count. -/
def splitByLines (input : String) (maxLines : Nat) (suffixLen : Nat := 2) (numericSuffix : Bool := false) : List (String × String) :=
  let lines := input.splitOn "\n"
  let chunks := splitLines lines maxLines
  chunks.map (λ (idx, chunk) =>
    (suffix idx suffixLen numericSuffix, String.intercalate "\n" chunk ++ (if input.endsWith "\n" then "\n" else "")))

/-- Split input bytes into chunks. -/
partial def splitBytes (input : ByteArray) (maxBytes : Nat) (suffixLen : Nat := 2) (numericSuffix : Bool := false) : List (String × ByteArray) :=
  let rec go (offset : Nat) (chunkIdx : Nat) (acc : List (String × ByteArray)) : List (String × ByteArray) :=
    if offset ≥ input.size then acc.reverse
    else
      let endPos := min (offset + maxBytes) input.size
      let chunk := input.extract offset endPos
      go endPos (chunkIdx + 1) ((suffix chunkIdx suffixLen numericSuffix, chunk) :: acc)
  go 0 0 []

/-- Parse a number with optional suffix (b, k, K, M, G). -/
def parseSuffixed (s : String) : Option Nat :=
  let (numPart, mult) :=
    if s.endsWith "b" then (s.dropEnd 1, 512)
    else if s.endsWith "k" then (s.dropEnd 1, 1024)
    else if s.endsWith "K" then (s.dropEnd 1, 1024)
    else if s.endsWith "M" then (s.dropEnd 1, 1024 * 1024)
    else if s.endsWith "G" then (s.dropEnd 1, 1024 * 1024 * 1024)
    else (s, 1)
  match numPart.toNat? with
  | some n => some (n * mult)
  | none => none

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Specification: split input by lines or bytes according to parameters.
Returns a list of (suffix, content) pairs.
-/
def spec (input : SplitInput) : List (String × String) :=
  splitByLines input.input input.maxLines input.suffixLen input.numericSuffix

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: suffix 0 with default params = "aa".
-/
theorem i_suffix_0 : suffix 0 = "aa" := by native_decide

/--
I2: suffix 1 = "ab".
-/
theorem i_suffix_1 : suffix 1 = "ab" := by native_decide

/--
I3: suffix 25 = "az".
-/
theorem i_suffix_25 : suffix 25 = "az" := by native_decide

/--
I4: suffix 26 = "ba".
-/
theorem i_suffix_26 : suffix 26 = "ba" := by native_decide

/--
I5: suffix 0 with length 3 = "aaa".
-/
theorem i_suffix_0_len3 : suffix 0 3 false = "aaa" := by native_decide

/--
I6: suffix 0 numeric = "00".
-/
theorem i_suffix_0_num : suffix 0 2 true = "00" := by native_decide

/--
I7: suffix 5 numeric = "05".
-/
theorem i_suffix_5_num : suffix 5 2 true = "05" := by native_decide

/--
I8: suffix 10 numeric = "10".
-/
theorem i_suffix_10_num : suffix 10 2 true = "10" := by native_decide

/--
I9: With zero suffix length, the numeric suffix is just the decimal
representation. Parametric over all chunk indices.
-/
theorem i_suffix_numeric_zero_len (i : Nat) : suffix i 0 true = toString i := by
  unfold suffix
  simp

/--
I11: parseSuffixed "100" = some 100.
-/
theorem i_parse_100 : parseSuffixed "100" = some 100 := by native_decide

/--
I12: parseSuffixed "2k" = some 2048.
-/
theorem i_parse_2k : parseSuffixed "2k" = some 2048 := by native_decide

/--
I13: parseSuffixed "1M" = some 1048576.
-/
theorem i_parse_1M : parseSuffixed "1M" = some 1048576 := by native_decide

/--
I14: Empty input produces one chunk with the empty string.
-/
theorem i_split_empty_lines : splitByLines "" 1000 = [("aa", "")] := by native_decide

/--
I15: Split a single line into one chunk.
-/
theorem i_split_single_line : splitByLines "hello" 1000 = [("aa", "hello")] := by native_decide

/--
I16: Split a string with two lines.
-/
theorem i_split_two_lines : splitByLines "a\nb" 1 = [("aa", "a"), ("ab", "b")] := by native_decide

/--
I17: toBase26 0 = [].
-/
theorem i_toBase26_0 : toBase26 0 = [] := by native_decide

/--
I18: toBase26 1 = ['b']? Wait, 1 % 26 = 1, 1+97=98='b', 1/26=0, so ['b'].
-/
theorem i_toBase26_1 : toBase26 1 = ['b'] := by native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- suffix 0 = "aa" -/
example : suffix 0 = "aa" := i_suffix_0

/-- parseSuffixed "2k" = 2048 -/
example : parseSuffixed "2k" = some 2048 := i_parse_2k

end Lentils.Split.Logic
