/-
Sort.Logic — Pure line sorting for `sort`. 0BSD
-/

import Lentils.Common.Lines

namespace Lentils.Sort.Logic

open Lentils.Common.Lines
open ByteArray

def parseArgs (args : List String) : Bool × List String :=
  let rec go (args : List String) (reverse : Bool) : Bool × List String :=
    match args with
    | [] => (reverse, [])
    | "-r" :: rest => go rest true
    | "--reverse" :: rest => go rest true
    | arg :: rest =>
      if arg.startsWith "-" && arg ≠ "-" then go rest reverse
      else (reverse, arg :: rest)
  go args false

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

def insertionSort (lines : List ByteArray) : List ByteArray :=
  let rec insert (x : ByteArray) (sorted : List ByteArray) : List ByteArray :=
    match sorted with
    | [] => [x]
    | y :: ys => if byteArrayLT x y then x :: y :: ys else y :: insert x ys
  match lines with
  | [] => []
  | x :: xs => insert x (insertionSort xs)

def sortLines (ba : ByteArray) (reverse : Bool) : ByteArray :=
  let lines := splitLines ba
  -- Drop trailing empty line if present (POSIX: trailing newline produces empty final element)
  let cleaned :=
    match lines.reverse with
    | [] => []
    | last :: rest =>
      if last.isEmpty then rest.reverse else lines
  let sorted := insertionSort cleaned
  let final := if reverse then sorted.reverse else sorted
  joinLines final

-- Theorems

example : byteArrayLT (ByteArray.mk #[0x41]) (ByteArray.mk #[0x42]) = true := by native_decide
example : byteArrayLT (ByteArray.mk #[0x42]) (ByteArray.mk #[0x41]) = false := by native_decide
example : byteArrayLT ByteArray.empty (ByteArray.mk #[0x41]) = true := by native_decide
example : byteArrayLT (ByteArray.mk #[0x41, 0x42]) (ByteArray.mk #[0x41, 0x43]) = true := by native_decide

theorem sortLines_empty : sortLines ByteArray.empty false = ByteArray.empty := by native_decide
theorem sortLines_single : sortLines (ByteArray.mk #[0x41, 0x42]) false = ByteArray.mk #[0x41, 0x42] := by native_decide

end Lentils.Sort.Logic
