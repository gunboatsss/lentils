/-
Sort.Logic — Pure line sorting for `sort`. 0BSD
-/

import Lentils.Common.Lines

namespace Lentils.Sort.Logic

open Lentils.Common.Lines
open ByteArray

def parseArgs (args : List String) : Bool × Bool × List String :=
  let rec go (args : List String) (reverse : Bool) (numeric : Bool) : Bool × Bool × List String :=
    match args with
    | [] => (reverse, numeric, [])
    | "-r" :: rest => go rest true numeric
    | "--reverse" :: rest => go rest true numeric
    | "-n" :: rest => go rest reverse true
    | "--numeric-sort" :: rest => go rest reverse true
    | "-nr" :: rest => go rest true true
    | "-rn" :: rest => go rest true true
    | arg :: rest =>
      if arg.startsWith "-" && arg ≠ "-" then go rest reverse numeric
      else (reverse, numeric, arg :: rest)
  go args false false

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
    else b.size > 0
  termination_by a.size - i
  go 0

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

def insertionSort (lines : List ByteArray) (numeric : Bool) : List ByteArray :=
  let lt := if numeric then numericLT else byteArrayLT
  let rec insert (x : ByteArray) (sorted : List ByteArray) : List ByteArray :=
    match sorted with
    | [] => [x]
    | y :: ys => if lt x y then x :: y :: ys else y :: insert x ys
  match lines with
  | [] => []
  | x :: xs => insert x (insertionSort xs numeric)

def sortLines (ba : ByteArray) (reverse : Bool) (numeric : Bool) : ByteArray :=
  let lines := splitLines ba
  -- Drop trailing empty line if present (POSIX: trailing newline produces empty final element)
  let cleaned :=
    match lines.reverse with
    | [] => []
    | last :: rest =>
      if last.isEmpty then rest.reverse else lines
  let sorted := insertionSort cleaned numeric
  let final := if reverse then sorted.reverse else sorted
  joinLines final

-- Theorems

example : byteArrayLT (ByteArray.mk #[0x41]) (ByteArray.mk #[0x42]) = true := by native_decide
example : byteArrayLT (ByteArray.mk #[0x42]) (ByteArray.mk #[0x41]) = false := by native_decide
example : byteArrayLT ByteArray.empty (ByteArray.mk #[0x41]) = true := by native_decide
example : byteArrayLT (ByteArray.mk #[0x41, 0x42]) (ByteArray.mk #[0x41, 0x43]) = true := by native_decide

theorem sortLines_empty : sortLines ByteArray.empty false false = ByteArray.empty := by native_decide
theorem sortLines_single : sortLines (ByteArray.mk #[0x41, 0x42]) false false = ByteArray.mk #[0x41, 0x42] := by native_decide

-- parseIntLeading: numeric lines parse correctly
example : parseIntLeading (ByteArray.mk #[0x31, 0x30]) = 10 := by native_decide
example : parseIntLeading (ByteArray.mk #[0x32]) = 2 := by native_decide
example : parseIntLeading (ByteArray.mk #[0x2D, 0x35]) = (-5 : Int) := by native_decide
example : parseIntLeading (ByteArray.mk #[0x61, 0x62, 0x63]) = 0 := by native_decide
example : parseIntLeading ByteArray.empty = 0 := by native_decide

-- numericLT: numeric comparison works
example : numericLT (ByteArray.mk #[0x32]) (ByteArray.mk #[0x31, 0x30]) = true := by native_decide
example : numericLT (ByteArray.mk #[0x31]) (ByteArray.mk #[0x32]) = true := by native_decide

-- sortLines with -n: numeric sort (verified via shell tests)

end Lentils.Sort.Logic
