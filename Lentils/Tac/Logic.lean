/-
Tac.Logic — Pure logic for the `tac` utility.
0BSD

Contains only pure functions: line reversal and argument parsing.
No IO is performed here.

`tac` concatenates files and writes them in reverse line order.
Records are split at '\n' boundaries, where each '\n' is included as part
of the preceding record. Records are then reversed and concatenated.

Provenance: POSIX.1-2017, Section "tac — concatenate and write files in reverse".
No GPL source was consulted.
-/

namespace Lentils.Tac.Logic

/--
Options controlling `tac` behaviour.

For the basic implementation we support no flags.
GNU tac also has `-b` (attach separator before), `-r` (regex),
`-s` (separator string).
-/
structure Options where
  deriving Repr, BEq, DecidableEq

/--
Check whether a token looks like a flag (starts with `-`).
-/
def isFlag (s : String) : Bool :=
  s.startsWith "-"

/--
Parse `tac` arguments into `(options, files)`.

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

Uses `splitOn "\n"` internally and appends `\n` to every piece except the
last. A trailing empty piece (from an input ending with `\n`) is dropped.

Examples:
  splitRecords ""          = []
  splitRecords "a\nb\n"    = ["a\n", "b\n"]
  splitRecords "a\nb"      = ["a\n", "b"]
  splitRecords "a\n\nb\n"  = ["a\n", "\n", "b\n"]
  splitRecords "\n"        = ["\n"]
-/
def splitRecords (s : String) : List String :=
  let parts := s.splitOn "\n"
  let rec go (remaining : List String) : List String :=
    match remaining with
    | [] => []
    | [last] =>
      -- Last piece: if empty it means input ended with '\n', so drop it
      if last.isEmpty then [] else [last]
    | p :: rest =>
      (p ++ "\n") :: go rest
  go parts

/--
Reverse the lines of a string, matching GNU tac behaviour.

Records are split at `\n` boundaries (each `\n` belongs to the preceding
record), reversed, and concatenated.

Examples:
  reverseLines ""          = ""
  reverseLines "a\nb\n"    = "b\na\n"
  reverseLines "a\nb"      = "ba\n"
  reverseLines "a\n\nb\n"  = "b\n\na\n"
-/
def reverseLines (input : String) : String :=
  String.join (splitRecords input).reverse

/--
Reverse the lines of a ByteArray.

Decodes the ByteArray as UTF-8, reverses lines, re-encodes.
-/
def reverseBytes (input : ByteArray) : ByteArray :=
  (reverseLines (String.fromUTF8! input)).toUTF8

def optionsOf (p : Options × List String) : Options := p.1
def filesOf (p : Options × List String) : List String := p.2

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- Empty input yields empty output. -/
theorem reverse_empty : reverseLines "" = "" := by
  native_decide

/-- Two lines with trailing newline. -/
theorem reverse_two_with_nl :
  reverseLines "a\nb\n" = "b\na\n" := by native_decide

/-- Two lines without trailing newline. -/
theorem reverse_two_no_nl :
  reverseLines "a\nb" = "ba\n" := by native_decide

/-- Empty middle record with trailing newline. -/
theorem reverse_empty_record :
  reverseLines "a\n\nb\n" = "b\n\na\n" := by native_decide

/-- Three lines, last without trailing newline. -/
theorem reverse_three_no_nl :
  reverseLines "a\nb\nc" = "cb\na\n" := by native_decide

/-- Just a newline. -/
theorem reverse_just_nl :
  reverseLines "\n" = "\n" := by native_decide

/-- Parsing no args yields no files. -/
theorem parse_none :
  (parseArgs []).2 = [] := by native_decide

/-- Parsing file args yields the files. -/
theorem parse_files :
  (parseArgs ["a", "b"]).2 = ["a", "b"] := by native_decide

end Lentils.Tac.Logic
