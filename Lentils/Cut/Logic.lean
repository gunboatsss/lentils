/-
Cut.Logic — Pure field/character selection for `cut`. 0BSD
-/

import Lentils.Common.Lines

namespace Lentils.Cut.Logic

open Lentils.Common.Lines
open ByteArray

inductive Mode | fields | chars deriving Inhabited, BEq

structure Range where
  start : Option Nat
  stop  : Option Nat
deriving Inhabited, DecidableEq

structure Config where
  mode     : Mode := Mode.fields
  delim    : UInt8 := 0x09
  ranges   : List Range := []
  suppress : Bool := false
deriving Inhabited

def charAt (cs : List Char) (idx : Nat) : Option Char :=
  match cs, idx with
  | [], _ => none
  | c :: _, 0 => some c
  | _ :: rest, n+1 => charAt rest n

def parseRange (s : String) : Option Range :=
  let chars := s.toList
  let dashPos := chars.findIdx? (· == '-')
  match dashPos with
  | none =>
    match s.toNat? with
    | some n => some { start := some n, stop := some n }
    | none   => none
  | some 0 =>
    let rest := String.join ((chars.drop 1).map (λ c => String.singleton c))
    match rest.toNat? with
    | some n => some { start := some 1, stop := some n }
    | none   => none
  | some i =>
    match charAt chars i with
    | some '-' =>
      let leftChars := chars.take i
      let rightChars := chars.drop (i + 1)
      let left := String.join (leftChars.map (λ c => String.singleton c))
      let right := String.join (rightChars.map (λ c => String.singleton c))
      if right.isEmpty then
        match left.toNat? with
        | some n => some { start := some n, stop := none }
        | none   => none
      else
        match left.toNat?, right.toNat? with
        | some n, some m => some { start := some n, stop := some m }
        | _, _ => none
    | _ => none

def parseRangeList (s : String) : List Range :=
  let parts := s.splitOn ","
  parts.filterMap parseRange

def indexInRanges (ranges : List Range) (i : Nat) : Bool :=
  ranges.any (λ r =>
    (match r.start with | none => true | some s => i ≥ s) &&
    (match r.stop  with | none => true | some s => i ≤ s))

def parseArgs (args : List String) : Config :=
  let rec go (args : List String) (cfg : Config) : Config :=
    match args with
    | [] => cfg
    | "-f" :: fld :: rest => go rest { cfg with mode := Mode.fields, ranges := parseRangeList fld }
    | "-c" :: chs :: rest => go rest { cfg with mode := Mode.chars, ranges := parseRangeList chs }
    | "-s" :: rest => go rest { cfg with suppress := true }
    | "-d" :: d :: rest =>
      match d.toList with
      | [] => go rest cfg
      | c :: _ => go rest { cfg with delim := c.toUInt8 }
    | arg :: rest =>
      if arg.startsWith "-f" && arg ≠ "-f" then
        go rest { cfg with mode := Mode.fields, ranges := parseRangeList ((arg.drop 2).toString) }
      else if arg.startsWith "-c" && arg ≠ "-c" then
        go rest { cfg with mode := Mode.chars, ranges := parseRangeList ((arg.drop 2).toString) }
      else if arg.startsWith "-d" && arg ≠ "-d" then
        let dchar := (arg.drop 2).toString
        match dchar.toList with
        | [] => go rest cfg
        | c :: _ => go rest { cfg with delim := c.toUInt8 }
      else go rest cfg
  go args {}

def splitFields (ba : ByteArray) (delim : UInt8) : List ByteArray :=
  let rec go (i : Nat) (current : ByteArray) : List ByteArray :=
    if i < ba.size then
      let b := ba.get! i
      if b == delim then current :: go (i + 1) ByteArray.empty
      else go (i + 1) (current.push b)
    else [current]
  go 0 ByteArray.empty

def enumerateOne (xs : List α) : List (Nat × α) :=
  let rec go (xs : List α) (i : Nat) : List (Nat × α) :=
    match xs with
    | [] => []
    | x :: xs => (i, x) :: go xs (i + 1)
  go xs 1

def selectFields (line : ByteArray) (ranges : List Range) (delim : UInt8) : ByteArray :=
  let fields := splitFields line delim
  let indexed := enumerateOne fields
  let selected := indexed.filterMap (λ (idx, f) =>
    if indexInRanges ranges idx then some f else none)
  if selected.isEmpty then
    if line.isEmpty then ByteArray.empty else line
  else
    let rec joinDelim (flds : List ByteArray) : ByteArray :=
      match flds with
      | [] => ByteArray.empty
      | [x] => x
      | x :: xs => xs.foldl (λ acc ba => acc.push delim ++ ba) x
    joinDelim selected

def selectChars (line : ByteArray) (ranges : List Range) : ByteArray :=
  if ranges.isEmpty then
    line
  else
    let rec go (i : Nat) (acc : ByteArray) : ByteArray :=
      if i < line.size then
        let idx : Nat := i + 1
        if indexInRanges ranges idx then
          go (i + 1) (acc.push (line.get! i))
        else
          go (i + 1) acc
      else acc
    go 0 ByteArray.empty

def processLine (line : ByteArray) (cfg : Config) : ByteArray :=
  match cfg.mode with
  | Mode.fields => selectFields line cfg.ranges cfg.delim
  | Mode.chars  => selectChars line cfg.ranges

def contains (ba : ByteArray) (b : UInt8) : Bool :=
  let rec go (i : Nat) : Bool :=
    if i < ba.size then
      if ba.get! i == b then true else go (i + 1)
    else false
  go 0

def processInput (input : ByteArray) (cfg : Config) : ByteArray :=
  let lines := splitLines input
  let resultLines := lines.filterMap (λ l =>
    if cfg.suppress && cfg.mode == Mode.fields && ¬ (contains l cfg.delim) then
      none
    else
      some (processLine l cfg))
  joinLines resultLines

-- Theorems

example : parseRange "3" = some { start := some 3, stop := some 3 } := by native_decide
example : parseRange "3-5" = some { start := some 3, stop := some 5 } := by native_decide
example : parseRange "3-" = some { start := some 3, stop := none } := by native_decide
example : parseRange "-5" = some { start := some 1, stop := some 5 } := by native_decide
example : parseRange "abc" = none := by native_decide

example : processInput ByteArray.empty {} = ByteArray.empty := by native_decide

example : selectFields (ByteArray.mk #[0x41, 0x42]) [] 0x09 = ByteArray.mk #[0x41, 0x42] := by native_decide

example : selectChars (ByteArray.mk #[0x41, 0x42, 0x43]) [] = ByteArray.mk #[0x41, 0x42, 0x43] := by native_decide

example : selectChars (ByteArray.mk #[0x41, 0x42, 0x43]) [{ start := some 1, stop := some 1 }] = ByteArray.mk #[0x41] := by native_decide

example : indexInRanges [] 5 = false := rfl
example : indexInRanges [{ start := some 2, stop := some 4 }] 3 = true := rfl
example : indexInRanges [{ start := some 2, stop := some 4 }] 5 = false := rfl

example : splitFields ByteArray.empty 0x09 = [ByteArray.empty] := by native_decide

end Lentils.Cut.Logic
