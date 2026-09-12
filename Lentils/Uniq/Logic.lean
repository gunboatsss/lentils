/-
Uniq.Logic — Verified pure logic for `uniq`. 0BSD
-/

import Lentils.Common.Lines

namespace Lentils.Uniq.Logic

open Lentils.Common.Lines
open ByteArray

inductive Mode
  | normal
  | unique
  | repeated
  deriving Inhabited, DecidableEq

structure UniqInput where
  input : ByteArray := ByteArray.empty
  mode : Mode := Mode.normal
  deriving Inhabited

abbrev UniqOutput := ByteArray

def parseArgs (args : List String) : Mode × List String :=
  let rec go (args : List String) (mode : Mode) : Mode × List String :=
    match args with
    | [] => (mode, [])
    | "-d" :: rest => go rest Mode.repeated
    | "-u" :: rest => go rest Mode.unique
    | arg :: rest => if arg.startsWith "-" && arg ≠ "-" then go rest mode else (mode, arg :: rest)
  go args Mode.normal

def groupAdjacentDuplicates (lines : List ByteArray) : List (ByteArray × Nat) :=
  let rec go (lines : List ByteArray) (current : ByteArray) (count : Nat) : List (ByteArray × Nat) :=
    match lines with
    | [] => [(current, count)]
    | l :: rest => if l = current then go rest current (count + 1) else (current, count) :: go rest l 1
  match lines with | [] => [] | first :: rest => go rest first 1

def processLines (input : ByteArray) (mode : Mode) : ByteArray :=
  let lines := splitLines input
  let groups := groupAdjacentDuplicates lines
  let filtered : List ByteArray :=
    match mode with
    | Mode.normal   => groups.map (λ (h, _) => h)
    | Mode.unique   => (groups.filter (λ (_, c) => c = 1)).map (λ (h, _) => h)
    | Mode.repeated => (groups.filter (λ (_, c) => c > 1)).map (λ (h, _) => h)
  joinLines filtered

def spec (input : UniqInput) : UniqOutput :=
  processLines input.input input.mode

/--
A singleton line forms a single group with count 1 (parametric over all lines).
-/
theorem groupAdjacent_singleton (l : ByteArray) :
    groupAdjacentDuplicates [l] = [(l, 1)] := by
  unfold groupAdjacentDuplicates; rfl

/--
I2: parseArgs recognizes -d flag.
-/
theorem i_parseArgs_repeated : parseArgs ["-d"] = (Mode.repeated, []) := by
  native_decide

/--
I3: parseArgs recognizes -u flag.
-/
theorem i_parseArgs_unique : parseArgs ["-u"] = (Mode.unique, []) := by
  native_decide

/--
I4: parseArgs default is normal mode.
-/
theorem i_parseArgs_default : parseArgs [] = (Mode.normal, []) := rfl

/--
I5: Empty input yields empty output in normal mode.
-/
theorem i_empty_normal : processLines ByteArray.empty Mode.normal = ByteArray.empty := by
  native_decide

/--
I6: groupAdjacentDuplicates on empty list returns [].
-/
theorem i_groupAdjacent_empty : groupAdjacentDuplicates ([] : List ByteArray) = [] := rfl

example : processLines ByteArray.empty Mode.normal = ByteArray.empty := i_empty_normal
example : processLines (ByteArray.mk #[0x61, 0x0A, 0x61, 0x0A, 0x62]) Mode.normal = ByteArray.mk #[0x61, 0x0A, 0x62] := by native_decide
example : processLines (ByteArray.mk #[0x61, 0x0A, 0x62, 0x0A, 0x62]) Mode.unique = ByteArray.mk #[0x61] := by native_decide
example : processLines (ByteArray.mk #[0x61, 0x0A, 0x62, 0x0A, 0x62]) Mode.repeated = ByteArray.mk #[0x62] := by native_decide

end Lentils.Uniq.Logic
