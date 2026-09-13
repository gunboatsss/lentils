/-
Sort.Logic — Verified pure logic for `sort`. 0BSD
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
  deriving Inhabited, Repr, DecidableEq

structure SortOptions where
  reverse : Bool := false
  numeric : Bool := false
  unique : Bool := false
  separator : Option Char := none
  key : Option SortKey := none
  filenames : List String := []
  deriving Inhabited, Repr

structure SortInput where
  input : ByteArray := ByteArray.empty
  options : SortOptions := {}
  deriving Inhabited

abbrev SortOutput := ByteArray

def parseKeyDef (s : String) : Option SortKey :=
  if s.isEmpty then none
  else
    let numeric := s.endsWith "n"
    let core := if numeric then (s.dropEnd 1).toString else s
    let parts := core.splitOn ","
    match parts with
    | [part1] => match part1.toNat? with | some f => if f > 0 then some { startField := f, endField := none, numeric := numeric } else none | none => none
    | [part1, part2] =>
      match part1.toNat?, part2.toNat? with
      | some f1, some f2 => if f1 > 0 && f2 > 0 then some { startField := f1, endField := some f2, numeric := numeric } else none
      | _, _ => none
    | _ => none

def parseArgs (args : List String) : SortOptions :=
  let rec setFlag (opts : SortOptions) (c : Char) : SortOptions :=
    match c with | 'r' => { opts with reverse := true } | 'n' => { opts with numeric := true } | 'u' => { opts with unique := true } | _ => opts
  let rec go (args' : List String) (opts : SortOptions) : SortOptions :=
    match args' with
    | [] => opts
    | "--reverse" :: rest => go rest { opts with reverse := true }
    | "--numeric-sort" :: rest => go rest { opts with numeric := true }
    | "--unique" :: rest => go rest { opts with unique := true }
    | "-t" :: sepArg :: rest => match sepArg.toList with | c :: _ => go rest { opts with separator := some c } | [] => go rest opts
    | "-k" :: keyArg :: rest => go rest { opts with key := parseKeyDef keyArg }
    | arg :: rest =>
      if arg.startsWith "-t" && arg.length > 2 then
        match (arg.drop 2).toString.toList with | c :: _ => go rest { opts with separator := some c } | [] => go rest opts
      else if arg.startsWith "-k" && arg.length > 2 then
        go rest { opts with key := parseKeyDef ((arg.drop 2).toString) }
      else if arg.startsWith "-" && arg.length > 1 && !arg.startsWith "--" then
        let chars := arg.toList.drop 1
        go rest (chars.foldl setFlag opts)
      else go rest { opts with filenames := opts.filenames ++ [arg] }
  go args {}

def byteArrayLT (a b : ByteArray) : Bool :=
  let rec go (i : Nat) : Bool :=
    if i < a.size then
      if i < b.size then
        let ba := a.get! i; let bb := b.get! i
        if ba < bb then true else if ba > bb then false else go (i + 1)
      else false
    else i < b.size
  termination_by a.size - i
  go 0

def byteArrayCompare (a b : ByteArray) : Ordering :=
  if byteArrayLT a b then Ordering.lt else if byteArrayLT b a then Ordering.gt else Ordering.eq

def parseIntLeading (ba : ByteArray) : Int :=
  let rec go (i : Nat) (acc : Int) (neg : Bool) : Int :=
    if i < ba.size then
      let b := ba.get! i
      if b.toNat >= 0x30 && b.toNat <= 0x39 then go (i + 1) (acc * 10 + (Int.ofNat (b.toNat - 0x30))) neg
      else acc * (if neg then -1 else 1)
    else acc * (if neg then -1 else 1)
  termination_by ba.size - i
  -- GNU numeric comparison skips leading blanks (space/tab).
  let rec skip (i : Nat) : Nat :=
    if i < ba.size then
      let b := (ba.get! i).toNat
      if b == 0x20 || b == 0x09 then skip (i + 1) else i
    else i
  termination_by ba.size - i
  let s := skip 0
  if s < ba.size then
    let first := ba.get! s
    if first.toNat = 0x2D then go (s + 1) 0 true
    else if first.toNat = 0x2B then go (s + 1) 0 false
    else if first.toNat >= 0x30 && first.toNat <= 0x39 then go s 0 false
    else 0
  else 0

def extractKey (line : ByteArray) (sep : Char) (key : SortKey) : ByteArray :=
  let sepByte : UInt8 := UInt8.ofNat sep.toNat
  let fields := splitOn line sepByte
  let startIdx := key.startField - 1
  if startIdx >= fields.length then ByteArray.empty
  else
    let count := match key.endField with | none => fields.length - startIdx | some ef => let endIdx := ef - 1; if endIdx < startIdx then 0 else endIdx - startIdx + 1
    let rec takeRange (fs : List ByteArray) (skip : Nat) (take : Nat) : List ByteArray :=
      match fs with | [] => [] | f :: rest => if skip > 0 then takeRange rest (skip - 1) take else if take > 0 then f :: takeRange rest 0 (take - 1) else []
    joinWith (takeRange fields startIdx count) sepByte

def compareLines (opts : SortOptions) (a b : ByteArray) : Ordering :=
  let primaryCmp := match opts.key, opts.separator with
    | some k, some sep =>
      let ka := extractKey a sep k; let kb := extractKey b sep k
      if k.numeric then
        let na := parseIntLeading ka; let nb := parseIntLeading kb
        if na < nb then Ordering.lt else if na > nb then Ordering.gt else byteArrayCompare ka kb
      else byteArrayCompare ka kb
    | _, _ =>
      if opts.numeric then
        let na := parseIntLeading a; let nb := parseIntLeading b
        if na < nb then Ordering.lt else if na > nb then Ordering.gt else byteArrayCompare a b
      else byteArrayCompare a b
  if primaryCmp = Ordering.eq && opts.key.isSome then byteArrayCompare a b else primaryCmp

def dedupLines (lines : List ByteArray) (eq : ByteArray → ByteArray → Bool) : List ByteArray :=
  match lines with | [] => [] | [x] => [x] | x :: y :: rest => if eq x y then dedupLines (x :: rest) eq else x :: dedupLines (y :: rest) eq

/--
Equality for `-u`: GNU dedups on key equality, ignoring the full-line
tie-break used for ordering. Without a key, numeric mode compares by
numeric value (`-n -u`: "1" and "01" are duplicates).
-/
def dedupEq (opts : SortOptions) (a b : ByteArray) : Bool :=
  match opts.key, opts.separator with
  | some k, some sep =>
    let ka := extractKey a sep k; let kb := extractKey b sep k
    if k.numeric then parseIntLeading ka == parseIntLeading kb
    else byteArrayCompare ka kb == Ordering.eq
  | _, _ =>
    if opts.numeric then parseIntLeading a == parseIntLeading b
    else compareLines opts a b == Ordering.eq

def insertionSort (lines : List ByteArray) (lt : ByteArray → ByteArray → Bool) : List ByteArray :=
  let rec insert (x : ByteArray) (sorted : List ByteArray) : List ByteArray :=
    match sorted with | [] => [x] | y :: ys => if lt x y then x :: y :: ys else y :: insert x ys
  match lines with | [] => [] | x :: xs => insert x (insertionSort xs lt)

def sortLines (ba : ByteArray) (opts : SortOptions) : ByteArray :=
  let lines := splitLines ba
  let cleaned := match lines.reverse with | [] => [] | last :: rest => if last.isEmpty then rest.reverse else lines
  let lt (a b : ByteArray) : Bool := compareLines opts a b = Ordering.lt
  let sorted := insertionSort cleaned lt
  -- Dedup before reversing: `-u` keeps the first of an equal run in
  -- sorted order, so `-r` cannot flip which duplicate survives.
  let deduped := if opts.unique then dedupLines sorted (dedupEq opts) else sorted
  let final := if opts.reverse then deduped.reverse else deduped
  joinLines final

def spec (input : SortInput) : SortOutput :=
  sortLines input.input input.options

/--
I1: Sorting empty input yields empty output.
-/
theorem i_empty : sortLines ByteArray.empty {} = ByteArray.empty := by
  native_decide

/--
I3: List.reverse is an involution on any list.
-/
theorem i_reverse_involution (lines : List ByteArray) : lines.reverse.reverse = lines := by
  simp

/--
I4: parseIntLeading on empty is 0.
-/
theorem i_parseInt_empty : parseIntLeading ByteArray.empty = 0 := rfl

/--
I5: parseKeyDef parses "2" correctly.
-/
theorem i_parseKeyDef_simple : parseKeyDef "2" = some { startField := 2, endField := none, numeric := false } := by
  native_decide

/--
I6: parseKeyDef parses "2,2n" correctly.
-/
theorem i_parseKeyDef_numeric : parseKeyDef "2,2n" = some { startField := 2, endField := some 2, numeric := true } := by
  native_decide

theorem joinLines_empty : joinLines ([] : List ByteArray) = ByteArray.empty := rfl

/--
insertionSort on the empty list is the empty list.
Parametric over the comparison function.
-/
theorem insertionSort_nil (lt : ByteArray → ByteArray → Bool) :
    insertionSort [] lt = [] := by simp [insertionSort]

/--
dedupLines on the empty list is the empty list.
Parametric over the equality function.
-/
theorem dedupLines_nil (eq : ByteArray → ByteArray → Bool) :
    dedupLines [] eq = [] := by simp [dedupLines]

/--
dedupLines on a singleton list is the singleton.
Parametric over the element and the equality function.
-/
theorem dedupLines_singleton (x : ByteArray) (eq : ByteArray → ByteArray → Bool) :
    dedupLines [x] eq = [x] := by simp [dedupLines]

example : sortLines ByteArray.empty {} = ByteArray.empty := i_empty
example : byteArrayLT (ByteArray.mk #[0x41]) (ByteArray.mk #[0x42]) = true := by native_decide
example : parseIntLeading (ByteArray.mk #[0x31, 0x30]) = 10 := by native_decide

end Lentils.Sort.Logic
