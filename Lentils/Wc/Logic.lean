/-
Wc.Logic — Verified pure logic for `wc`. 0BSD
-/

import Lentils.Common.Bytes

namespace Lentils.Wc.Logic

open Lentils.Common.Bytes
open ByteArray

structure Flags where
  lines : Bool := false
  words : Bool := false
  bytes : Bool := false
  deriving Inhabited, DecidableEq

def defaultFlags : Flags := { lines := true, words := true, bytes := true }

structure WcInput where
  content : ByteArray := ByteArray.empty
  flags : Flags := defaultFlags
  filename : String := ""
  deriving Inhabited

abbrev WcOutput := String

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

/--
Specification: count and format according to flags.
-/
def spec (input : WcInput) : WcOutput :=
  formatCounts (countLines input.content) (countWords input.content)
    (countBytes input.content) input.filename input.flags

/--
I1: Empty input → all counts zero.
-/
theorem i_empty_counts :
    countLines ByteArray.empty = 0 ∧ countWords ByteArray.empty = 0 ∧ countBytes ByteArray.empty = 0 := by
  native_decide

/--
I2: Byte count = size for all inputs.
-/
theorem i_bytes_eq_size (ba : ByteArray) : countBytes ba = ba.size := rfl

/--
I3: Default flags are all true.
-/
theorem i_defaultFlags_all_true : defaultFlags.lines = true ∧ defaultFlags.words = true ∧ defaultFlags.bytes = true := by
  decide

/--
I5: parseArgs empty → default flags.
-/
theorem i_parseArgs_empty : parseArgs [] = (defaultFlags, []) := by
  native_decide

/--
I6: parseArgs -l → lines-only.
-/
theorem i_parseArgs_lines : parseArgs ["-l"] = (Flags.mk true false false, []) := by
  native_decide

/--
I7: parseArgs -w → words-only.
-/
theorem i_parseArgs_words : parseArgs ["-w"] = (Flags.mk false true false, []) := by
  native_decide

/--
I8: parseArgs -c → bytes-only.
-/
theorem i_parseArgs_bytes : parseArgs ["-c"] = (Flags.mk false false true, []) := by
  native_decide

/--
I9: countWords empty = 0.
-/
theorem i_countWords_empty : countWords ByteArray.empty = 0 := by
  native_decide

/--
I10: countLines counts newlines in any byte array.
-/
theorem i_countLines_eq_newlines (ba : ByteArray) : countLines ba = countNewlines ba := rfl

theorem countBytes_empty : countBytes ByteArray.empty = 0 := rfl

/--
A singleton byte array has byte count 1 (parametric over all bytes).
-/
theorem countBytes_singleton : ∀ b : UInt8,
    countBytes (ByteArray.mk #[b]) = 1 := by
  intro b
  simp [countBytes]
  rfl

/--
Lines-only formatting renders just the line count plus newline (parametric over all counts).
-/
theorem formatCounts_lines_only : ∀ n : Nat,
    formatCounts n 0 0 "" { lines := true, words := false, bytes := false } =
    toString n ++ "\n" := by
  intro n
  simp [formatCounts]
  rfl

example : spec { content := ByteArray.empty, flags := defaultFlags, filename := "" } = "0 0 0\n" := by
  native_decide

example : formatCounts 1 0 0 "" (Flags.mk true false false) = "1\n" := by
  native_decide

example : countBytes (ByteArray.mk #[0x68, 0x65, 0x6C, 0x6C, 0x6F]) = 5 := by
  native_decide

end Lentils.Wc.Logic
