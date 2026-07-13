/-
Sort.Logic — Pure line sorting for `sort`. 0BSD
-/

import Lentils.Common.Lines
import Lentils.Common.Bytes

namespace Lentils.Sort.Logic

open Lentils.Common.Lines
open Lentils.Common.Bytes
open ByteArray

structure SortKey where
  startField : Nat
  endField : Option Nat
  numeric : Bool
  deriving Inhabited, Repr

structure SortOptions where
  reverse : Bool := false
  numeric : Bool := false
  unique : Bool := false
  separator : Option Char := none
  key : Option SortKey := none
  filenames : List String := []
  deriving Inhabited, Repr

/-- Parse a key definition string like "2", "2,2", "2n", "2,2n".
    Returns none if the format is invalid. -/
def parseKeyDef (s : String) : Option SortKey :=
  if s.isEmpty then none
  else
    let numeric := s.endsWith "n"
    let core := if numeric then (s.dropEnd 1).toString else s
    let parts := core.splitOn ","
    match parts with
    | [part1] =>
      match part1.toNat? with
      | some f => if f > 0 then some { startField := f, endField := none, numeric := numeric } else none
      | none => none
    | [part1, part2] =>
      match part1.toNat?, part2.toNat? with
      | some f1, some f2 =>
        if f1 > 0 && f2 > 0 then some { startField := f1, endField := some f2, numeric := numeric } else none
      | _, _ => none
    | _ => none

def parseArgs (args : List String) : SortOptions :=
  let rec setFlag (opts : SortOptions) (c : Char) : SortOptions :=
    match c with
    | 'r' => { opts with reverse := true }
    | 'n' => { opts with numeric := true }
    | 'u' => { opts with unique := true }
    | _   => opts

  let rec go (args' : List String) (opts : SortOptions) : SortOptions :=
    match args' with
    | [] => opts
    | "--reverse" :: rest => go rest { opts with reverse := true }
    | "--numeric-sort" :: rest => go rest { opts with numeric := true }
    | "-t" :: sepArg :: rest =>
      match sepArg.toList with
      | c :: _ => go rest { opts with separator := some c }
      | [] => go rest opts
    | "-k" :: keyArg :: rest =>
      let parsedKey := parseKeyDef keyArg
      go rest { opts with key := parsedKey }
    | arg :: rest =>
      if arg.startsWith "-t" && arg.length > 2 then
        let sepStr := (arg.drop 2).toString
        match sepStr.toList with
        | c :: _ => go rest { opts with separator := some c }
        | [] => go rest opts
      else if arg.startsWith "-k" && arg.length > 2 then
        let keyStr := (arg.drop 2).toString
        let parsedKey := parseKeyDef keyStr
        go rest { opts with key := parsedKey }
      else if arg.startsWith "-" && arg.length > 1 && !arg.startsWith "--" then
        -- Combined short flags: "-ru", "-nr", "-rnu", etc.
        let chars := arg.toList.drop 1
        let opts' := chars.foldl setFlag opts
        go rest opts'
      else
        go rest { opts with filenames := opts.filenames ++ [arg] }
  go args {}

def byteArrayLT (a b : ByteArray) : Bool :=
  let rec go (i : Nat) : Bool :=
    if i < a.size then
      if i < b.size then
        let ba := a.get! i
        let bb := b.get! i
        if ba < bb then true
        else if ba > bb then false
        else go (i + 1)
      else false
    else i < b.size
  termination_by a.size - i
  go 0

def byteArrayCompare (a b : ByteArray) : Ordering :=
  if byteArrayLT a b then Ordering.lt
  else if byteArrayLT b a then Ordering.gt
  else Ordering.eq

/-- Parse a leading (possibly negative) integer from the start of a ByteArray.
    Lines that do not start with a digit or '-' produce 0. -/
def parseIntLeading (ba : ByteArray) : Int :=
  let rec go (i : Nat) (acc : Int) (neg : Bool) : Int :=
    if i < ba.size then
      let b := ba.get! i
      if b.toNat >= 0x30 && b.toNat <= 0x39 then
        go (i + 1) (acc * 10 + (Int.ofNat (b.toNat - 0x30))) neg
      else acc * (if neg then -1 else 1)
    else acc * (if neg then -1 else 1)
  termination_by ba.size - i
  if ba.isEmpty then 0
  else
    let first := ba.get! 0
    if first.toNat = 0x2D then go 1 0 true
    else if first.toNat >= 0x30 && first.toNat <= 0x39 then go 0 0 false
    else 0

/-- Compare two ByteArrays by leading numeric value, falling back to
    lexicographic comparison for tie-breaking. -/
def numericLT (a b : ByteArray) : Bool :=
  let na := parseIntLeading a
  let nb := parseIntLeading b
  if na < nb then true
  else if na > nb then false
  else byteArrayLT a b

/-- Extract the key fields from a line using the given separator and key definition.
    Returns an empty ByteArray if the start field is past the end. -/
def extractKey (line : ByteArray) (sep : Char) (key : SortKey) : ByteArray :=
  let sepByte : UInt8 := UInt8.ofNat sep.toNat
  let fields := splitOn line sepByte
  let startIdx := key.startField - 1
  if startIdx >= fields.length then
    ByteArray.empty
  else
    let count := match key.endField with
      | none => fields.length - startIdx
      | some ef =>
        let endIdx := ef - 1
        if endIdx < startIdx then 0
        else endIdx - startIdx + 1
    let rec takeRange (fs : List ByteArray) (skip : Nat) (take : Nat) : List ByteArray :=
      match fs with
      | [] => []
      | f :: rest =>
        if skip > 0 then takeRange rest (skip - 1) take
        else if take > 0 then f :: takeRange rest 0 (take - 1)
        else []
    let selectedFields := takeRange fields startIdx count
    joinWith selectedFields sepByte

/-- Compare two lines according to SortOptions. Returns Ordering.
    When a key is specified, compares by key first, then falls back
    to whole-line comparison for ties. -/
def compareLines (opts : SortOptions) (a b : ByteArray) : Ordering :=
  let primaryCmp := match opts.key, opts.separator with
    | some k, some sep =>
      let ka := extractKey a sep k
      let kb := extractKey b sep k
      if k.numeric then
        let na := parseIntLeading ka
        let nb := parseIntLeading kb
        if na < nb then Ordering.lt
        else if na > nb then Ordering.gt
        else byteArrayCompare ka kb
      else
        byteArrayCompare ka kb
    | _, _ =>
      if opts.numeric then
        let na := parseIntLeading a
        let nb := parseIntLeading b
        if na < nb then Ordering.lt
        else if na > nb then Ordering.gt
        else byteArrayCompare a b
      else
        byteArrayCompare a b
  if primaryCmp = Ordering.eq && opts.key.isSome then
    byteArrayCompare a b
  else
    primaryCmp

/-- Remove adjacent duplicate lines. Keeps the first of each run of equal lines. -/
def dedupLines (lines : List ByteArray) (eq : ByteArray → ByteArray → Bool) : List ByteArray :=
  match lines with
  | [] => []
  | [x] => [x]
  | x :: y :: rest =>
    if eq x y then dedupLines (x :: rest) eq
    else x :: dedupLines (y :: rest) eq

def insertionSort (lines : List ByteArray) (lt : ByteArray → ByteArray → Bool) : List ByteArray :=
  let rec insert (x : ByteArray) (sorted : List ByteArray) : List ByteArray :=
    match sorted with
    | [] => [x]
    | y :: ys => if lt x y then x :: y :: ys else y :: insert x ys
  match lines with
  | [] => []
  | x :: xs => insert x (insertionSort xs lt)

def sortLines (ba : ByteArray) (opts : SortOptions) : ByteArray :=
  let lines := splitLines ba
  let hasTrailingNewline := !ba.isEmpty && ba.get! (ba.size - 1) = 0x0A
  -- Drop trailing empty line if present (POSIX: trailing newline produces empty final element)
  let cleaned :=
    match lines.reverse with
    | [] => []
    | last :: rest =>
      if last.isEmpty then rest.reverse else lines
  let lt (a b : ByteArray) : Bool := compareLines opts a b = Ordering.lt
  let eq (a b : ByteArray) : Bool := compareLines opts a b = Ordering.eq
  let sorted := insertionSort cleaned lt
  let final := if opts.reverse then sorted.reverse else sorted
  let deduped := if opts.unique then dedupLines final eq else final
  let result := joinLines deduped
  if hasTrailingNewline then result.push 0x0A else result

-- Theorems

example : byteArrayLT (ByteArray.mk #[0x41]) (ByteArray.mk #[0x42]) = true := by native_decide
example : byteArrayLT (ByteArray.mk #[0x42]) (ByteArray.mk #[0x41]) = false := by native_decide
example : byteArrayLT ByteArray.empty (ByteArray.mk #[0x41]) = true := by native_decide
example : byteArrayLT (ByteArray.mk #[0x41, 0x42]) (ByteArray.mk #[0x41, 0x43]) = true := by native_decide

theorem sortLines_empty : sortLines ByteArray.empty {} = ByteArray.empty := by native_decide
theorem sortLines_single : sortLines (ByteArray.mk #[0x41, 0x42]) {} = ByteArray.mk #[0x41, 0x42] := by native_decide

-- parseIntLeading: numeric lines parse correctly
example : parseIntLeading (ByteArray.mk #[0x31, 0x30]) = 10 := by native_decide
example : parseIntLeading (ByteArray.mk #[0x32]) = 2 := by native_decide
example : parseIntLeading (ByteArray.mk #[0x2D, 0x35]) = (-5 : Int) := by native_decide
example : parseIntLeading (ByteArray.mk #[0x61, 0x62, 0x63]) = 0 := by native_decide
example : parseIntLeading ByteArray.empty = 0 := by native_decide

-- numericLT: numeric comparison works
example : numericLT (ByteArray.mk #[0x32]) (ByteArray.mk #[0x31, 0x30]) = true := by native_decide
example : numericLT (ByteArray.mk #[0x31]) (ByteArray.mk #[0x32]) = true := by native_decide

-- extractKey tests
example : extractKey (ByteArray.mk #[0x61, 0x3A, 0x31]) ':' { startField := 2, endField := none, numeric := false } = ByteArray.mk #[0x31] := by
  native_decide

-- dedupLines tests
example : dedupLines ([ByteArray.mk #[0x61], ByteArray.mk #[0x62]] : List ByteArray) (λ a b => a = b) = [ByteArray.mk #[0x61], ByteArray.mk #[0x62]] := by
  native_decide

example : dedupLines ([ByteArray.mk #[0x61], ByteArray.mk #[0x61]] : List ByteArray) (λ a b => a = b) = [ByteArray.mk #[0x61]] := by
  native_decide

end Lentils.Sort.Logic
