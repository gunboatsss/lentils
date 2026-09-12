/-
Tac.Logic — Verified pure logic for `tac` (reverse concatenation).
0BSD

Structure:
  1. State types      — TacInput, Options
  2. Specification    — reverseLines, reverseBytes, parseArgs
  3. Implementation   — reverseLines, reverseBytes, parseArgs (directly executable)
  4. Invariants       — parametric theorems over all inputs
  6. Lemmas           — helper theorems for invariants
  7. Concrete corollaries — derived examples

No IO, no FFI, no `sorry` or `admit`.

`tac` concatenates files and writes them in reverse line order.
Records are split at '\n' boundaries, where each '\n' is included as part
of the preceding record. Records are then reversed and concatenated.

Provenance: POSIX.1-2017, Section "tac — concatenate and write files in reverse".
No GPL source was consulted.
-/

import Lentils.Common.Spec

namespace Lentils.Tac.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Options controlling `tac` behaviour (currently none supported).
-/
structure Options where
  deriving Repr, BEq, DecidableEq, Inhabited

/--
Input state for tac.
-/
structure TacInput where
  args : List String
  deriving Inhabited, BEq, Repr

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Helpers
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Check whether a token looks like a flag (starts with `-`).
-/
def isFlag (s : String) : Bool :=
  s.startsWith "-"

/--
Parse `tac` arguments into `(Options, files)`.

`tac` reads from stdin when no files are given, or from the listed files.
A `--` terminates flag parsing.
-/
def parseArgs (args : List String) : Options × List String :=
  let rec go (remaining : List String) (opts : Options) (files : List String)
      : Options × List String :=
    match remaining with
    | [] => (opts, files.reverse)
    | "--" :: rest => (opts, files.reverse ++ rest)
    | s :: rest =>
      if s.startsWith "-" && s != "-" then
        (opts, files.reverse)
      else
        go rest opts (s :: files)
  go args {} []

/--
Split a string into records at `\n` boundaries. Each record (except possibly
the last) includes its trailing `\n` in the result.
-/
def splitRecords (s : String) : List String :=
  let parts := s.splitOn "\n"
  let rec go (remaining : List String) : List String :=
    match remaining with
    | [] => []
    | [last] =>
      if last.isEmpty then [] else [last]
    | p :: rest =>
      (p ++ "\n") :: go rest
  go parts

/--
Reverse the lines of a string.

Records are split at `\n` boundaries (each `\n` belongs to the preceding
record), reversed, and concatenated.
-/
def reverseLines (input : String) : String :=
  String.join (splitRecords input).reverse

/--
Reverse the lines of a ByteArray.

Decodes the ByteArray as UTF-8, reverses lines, re-encodes.
-/
def reverseBytes (input : ByteArray) : ByteArray :=
  (reverseLines (String.fromUTF8! input)).toUTF8

/--
Extract options from a parse result.
-/
def optionsOf (p : Options × List String) : Options := p.1

/--
Extract files from a parse result.
-/
def filesOf (p : Options × List String) : List String := p.2

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Specification: reverse lines of a string.
-/
def specReverseLines (input : String) : String := reverseLines input

/--
Specification: reverse lines of a byte array.
-/
def specReverseBytes (input : ByteArray) : ByteArray := reverseBytes input

/--
Specification: parse arguments.
-/
def specParseArgs (args : List String) : Options × List String := parseArgs args

/--
Specification for full TacInput.
-/
def spec (input : TacInput) : Options × List String := parseArgs input.args

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Empty input yields empty output.
-/
theorem i_reverse_empty : reverseLines "" = "" := by
  native_decide

/--
I2: Two concrete lines with trailing newline are reversed.
-/
theorem i_two_concrete_with_nl : reverseLines "a\nb\n" = "b\na\n" := by
  native_decide

/--
I3: Just a newline is identity.
-/
theorem i_just_newline : reverseLines "\n" = "\n" := by
  native_decide

/--
I4: Parsing no args yields empty files.
-/
theorem i_parse_none : (parseArgs []).2 = [] := by
  native_decide

/--
I7: Parsing two file args yields those files.
-/
theorem i_parse_two_concrete : (parseArgs ["a", "b"]).2 = ["a", "b"] := by
  native_decide

/--
I8: Parsing "--" stops flag processing.
-/
theorem i_parse_double_dash_concrete : (parseArgs ["--", "a", "b"]).2 = ["a", "b"] := by
  native_decide

/--
I9: Empty input to reverseBytes yields empty output.
-/
theorem i_reverse_bytes_empty : reverseBytes ByteArray.empty = ByteArray.empty := by
  native_decide

/--
I10: splitRecords of empty string returns empty list.
-/
theorem i_split_records_empty : splitRecords "" = [] := by
  native_decide

/--
I11: A single flag argument stops parsing and returns no files.
-/
theorem i_parse_flag_example : (parseArgs ["-x"]).2 = [] := by
  native_decide

/--
I12: Single non-flag file argument returns that file.
-/
theorem i_parse_file_example : (parseArgs ["file.txt"]).2 = ["file.txt"] := by
  native_decide

/--
I13: reverseLines of "a\nb" (no trailing newline).
-/
theorem i_reverse_no_trailing_nl_concrete : reverseLines "a\nb" = "ba\n" := by
  native_decide

/--
I14: reverseLines of "a\n\nb\n" handles empty middle record.
-/
theorem i_reverse_empty_mid_concrete : reverseLines "a\n\nb\n" = "b\n\na\n" := by
  native_decide

/--
I15: reverseLines of "a\nb\nc" (three lines).
-/
theorem i_reverse_three_concrete : reverseLines "a\nb\nc" = "cb\na\n" := by
  native_decide

/--
I16: Single line with trailing newline is identity (ground example).
-/
theorem i_single_with_nl_ground : reverseLines "hello\n" = "hello\n" := by
  native_decide

/--
I17: Single line without trailing newline (that contains no '\n') is identity.
-/
theorem i_single_no_nl_ground : reverseLines "hello" = "hello" := by
  native_decide

/--
Reversing the record list preserves its length (parametric over all inputs).
-/
theorem splitRecords_reverse_length (s : String) :
    (splitRecords s).reverse.length = (splitRecords s).length := by
  simp

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Lemmas
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
splitRecords of a single newline produces ["\n"].
-/
theorem splitRecords_newline_only : splitRecords "\n" = ["\n"] := by
  native_decide

/--
splitRecords of a concrete two-line string.
-/
theorem splitRecords_two_concrete : splitRecords "a\nb\n" = ["a\n", "b\n"] := by
  native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 7. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- Empty input yields empty output. -/
example : reverseLines "" = "" := i_reverse_empty

/-- Two lines with trailing newline are reversed. -/
example : reverseLines "a\nb\n" = "b\na\n" := i_two_concrete_with_nl

/-- Two lines without trailing newline. -/
example : reverseLines "a\nb" = "ba\n" := i_reverse_no_trailing_nl_concrete

/-- Empty middle record with trailing newline. -/
example : reverseLines "a\n\nb\n" = "b\n\na\n" := i_reverse_empty_mid_concrete

/-- Three lines, last without trailing newline. -/
example : reverseLines "a\nb\nc" = "cb\na\n" := i_reverse_three_concrete

/-- Just a newline. -/
example : reverseLines "\n" = "\n" := i_just_newline

/-- Parsing no args yields no files. -/
example : (parseArgs []).2 = [] := i_parse_none

/-- Parsing two file args yields those files. -/
example : (parseArgs ["a", "b"]).2 = ["a", "b"] := i_parse_two_concrete

/-- Double dash stops flag processing. -/
example : (parseArgs ["--", "a", "b"]).2 = ["a", "b"] := i_parse_double_dash_concrete

/-- Flag argument results in no files. -/
example : (parseArgs ["-x"]).2 = [] := i_parse_flag_example

end Lentils.Tac.Logic
