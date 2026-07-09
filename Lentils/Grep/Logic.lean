/-
Grep.Logic — Pure pattern matching for `grep`. 0BSD
-/

import Lentils.Common.Lines

namespace Lentils.Grep.Logic

open Lentils.Common.Lines
open ByteArray

structure Flags where
  invert : Bool := false
deriving Inhabited, DecidableEq

def parseArgs (args : List String) : Flags × String × List String :=
  let rec go (args : List String) (flags : Flags) (pattern : String) : Flags × String × List String :=
    match args with
    | [] => (flags, pattern, [])
    | "-v" :: rest => go rest { flags with invert := true } pattern
    | "-e" :: p :: rest => go rest flags p
    | arg :: rest =>
      if arg.startsWith "-" && arg ≠ "-" && arg ≠ "-e" then go rest flags pattern
      else if pattern.isEmpty then go rest flags arg
      else (flags, pattern, arg :: rest)
  go args {} ""

def rangeEq (ba : ByteArray) (start : Nat) (sub : ByteArray) : Bool :=
  if start + sub.size > ba.size then false
  else
    let rec go (i : Nat) : Bool :=
      if i >= sub.size then true
      else if ba.get! (start + i) == sub.get! i then go (i + 1)
      else false
    go 0

def containsPattern (text : ByteArray) (pattern : ByteArray) : Bool :=
  if pattern.isEmpty then true
  else if pattern.size > text.size then false
  else
    let maxStart := text.size - pattern.size
    let rec check (pos : Nat) : Bool :=
      if pos > maxStart then false
      else if rangeEq text pos pattern then true
      else check (pos + 1)
    termination_by maxStart + 1 - pos
    check 0

def processInput (input : ByteArray) (pattern : String) (flags : Flags) : ByteArray × Bool :=
  let patternBytes := pattern.toUTF8
  let lines := splitLines input
  let matching := lines.filter (λ line =>
    let matched := containsPattern line patternBytes
    if flags.invert then ¬ matched else matched)
  (joinLines matching, !matching.isEmpty)

-- Theorems

example : containsPattern ByteArray.empty (ByteArray.mk #[0x41]) = false := by native_decide
example : containsPattern (ByteArray.mk #[0x41, 0x42]) ByteArray.empty = true := rfl
example : containsPattern (ByteArray.mk #[0x41, 0x42, 0x43]) (ByteArray.mk #[0x42]) = true := by native_decide
example : containsPattern (ByteArray.mk #[0x41, 0x42, 0x43]) (ByteArray.mk #[0x44]) = false := by native_decide

end Lentils.Grep.Logic
