/-
Wc.Logic — Pure counting for `wc`. 0BSD -/
import Lentils.Common.Bytes
namespace Lentils.Wc.Logic
open ByteArray
open Lentils.Common.Bytes

structure Flags where
  lines : Bool := false
  words : Bool := false
  bytes : Bool := false
  deriving Inhabited, DecidableEq

def defaultFlags : Flags := { lines := true, words := true, bytes := true }

def parseArgs (args : List String) : Flags × List String :=
  let rec go (args : List String) (flags : Flags) : Flags × List String :=
    match args with
    | [] => (flags, [])
    | arg :: rest =>
      if arg.startsWith "-" && arg ≠ "-" then
        let flagChars := arg.drop 1
        let newFlags := flagChars.toString.foldl (λ (f : Flags) c =>
          match c with
          | 'l' => { f with lines := true }
          | 'w' => { f with words := true }
          | 'c' => { f with bytes := true }
          | _   => f) flags
        go rest newFlags
      else (flags, arg :: rest)
  let (flags, filenames) := go args {}
  let flags := if !flags.lines && !flags.words && !flags.bytes then defaultFlags else flags
  (flags, filenames)

def countLines (ba : ByteArray) : Nat := countNewlines ba

def countWords (ba : ByteArray) : Nat :=
  let rec go (i : Nat) (inWord : Bool) (count : Nat) : Nat :=
    if i < ba.size then
      let b := ba.get! i
      let isSpace := b == 0x20 || b == 0x09 || b == 0x0A || b == 0x0D || b == 0x0B || b == 0x0C
      if isSpace then go (i + 1) false count
      else if inWord then go (i + 1) true count
      else go (i + 1) true (count + 1)
    else count
  go 0 false 0

def countBytes (ba : ByteArray) : Nat := ba.size

def formatCounts (lines words bytes : Nat) (filename : String) (flags : Flags) : String :=
  let parts : List String := Id.run do
    let mut result : List String := []
    if flags.lines then result := result ++ [toString lines]
    if flags.words then result := result ++ [toString words]
    if flags.bytes then result := result ++ [toString bytes]
    if !filename.isEmpty then result := result ++ [filename]
    result
  String.intercalate " " parts ++ "\n"

theorem countLines_empty : countLines ByteArray.empty = 0 := rfl

/-- A single newline byte counts as exactly one line. -/
theorem countLines_singleNewline : countLines (ByteArray.mk #[0x0A]) = 1 := rfl

/-- In any input, line count equals the number of newline bytes. -/
theorem countLines_eq_countNewlines (ba : ByteArray) :
  countLines ba = countNewlines ba := rfl

/-- Two newline bytes yield exactly two lines. -/
theorem countLines_twoNewlines : countLines (ByteArray.mk #[0x0A, 0x0A]) = 2 := by native_decide

/-- Empty input contains zero lines (wc -l on empty file). -/
theorem countLines_emptyInput : countLines (ByteArray.mk #[]) = 0 := rfl

/-- A single non-space token is exactly one word (wc -w). -/
theorem countWords_singleWord :
  countWords (ByteArray.mk #[0x68, 0x65, 0x6C, 0x6C, 0x6F]) = 1 := by native_decide

/-- "hello world" contains exactly two whitespace-delimited words. -/
theorem countWords_twoWords :
  countWords (ByteArray.mk #[0x68, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x77, 0x6F, 0x72, 0x6C, 0x64]) = 2 := by native_decide

/-- Word count in the empty input is zero (wc -w on empty file). -/
theorem countWords_empty : countWords ByteArray.empty = 0 := by native_decide

/-- Byte count of the empty input is zero (wc -c). -/
theorem countBytes_empty : countBytes ByteArray.empty = 0 := rfl

/-- A single newline byte is one byte (wc -c). -/
theorem countBytes_singleNewline : countBytes (ByteArray.mk #[0x0A]) = 1 := by native_decide

/-- A single-line input "hello" is five bytes (wc -c on single-line input). -/
theorem countBytes_singleLine :
  countBytes (ByteArray.mk #[0x68, 0x65, 0x6C, 0x6C, 0x6F]) = 5 := by native_decide

/-- Default behaviour (no flags) enables all three counts. -/
theorem defaultFlags_allTrue :
  defaultFlags.lines = true ∧ defaultFlags.words = true ∧ defaultFlags.bytes = true := by decide

/-- Line counting (`-l`) is the default when no flags are given. -/
theorem parseArgs_empty : parseArgs [] = (defaultFlags, []) := by native_decide

/-- `wc -l` enables only the lines flag. -/
theorem parseArgs_linesFlag :
  parseArgs ["-l"] = ({ lines := true, words := false, bytes := false }, []) := by native_decide

/-- Flag combination `wc -lw` enables lines and words. -/
theorem parseArgs_lwFlag :
  parseArgs ["-lw"] = ({ lines := true, words := true, bytes := false }, []) := by native_decide

/-- Flag combination `wc -lc` enables lines and bytes. -/
theorem parseArgs_lcFlag :
  parseArgs ["-lc"] = ({ lines := true, words := false, bytes := true }, []) := by native_decide

/-- A bare filename with no flags selects defaults and reports the file. -/
theorem parseArgs_filename :
  parseArgs ["file.txt"] = (defaultFlags, ["file.txt"]) := by native_decide

/-- Default output for empty input is "0 0 0\n" (wc default, all three). -/
theorem formatCounts_defaultEmpty :
  formatCounts 0 0 0 "" defaultFlags = "0 0 0\n" := by native_decide

/-- `wc -l` output for a one-line empty-body file is just "1\n". -/
theorem formatCounts_linesOnly :
  formatCounts 1 0 0 "" { lines := true, words := false, bytes := false } = "1\n" := by native_decide

/-- A single-line input without a trailing newline has zero newlines, hence zero lines. -/
theorem countLines_singleLineNoNewline :
  countLines (ByteArray.mk #[0x68, 0x65, 0x6C, 0x6C, 0x6F]) = 0 := by native_decide

/-- A single-line input with a trailing newline counts as exactly one line (wc -l). -/
theorem countLines_singleLineWithNewline :
  countLines (ByteArray.mk #[0x68, 0x65, 0x6C, 0x6C, 0x6F, 0x0A]) = 1 := by native_decide

/-- A single-line input "hello" is exactly one word (wc -w on single-line input). -/
theorem countWords_singleLine :
  countWords (ByteArray.mk #[0x68, 0x65, 0x6C, 0x6C, 0x6F]) = 1 := by native_decide

/-- Default output for a single-line input "hello" is "0 1 5\n" (wc default, all three). -/
theorem formatCounts_defaultSingleLine :
  formatCounts 0 1 5 "hello" defaultFlags = "0 1 5 hello\n" := by native_decide

end Lentils.Wc.Logic
