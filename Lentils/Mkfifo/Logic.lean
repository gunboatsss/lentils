/-
Mkfifo.Logic — Verified pure logic for `mkfifo`. 0BSD
Spec-first methodology.
-/

import Lentils.Common.Spec

namespace Lentils.Mkfifo.Logic

open Lentils.Common.Spec

structure Options where
  mode : UInt32 := 0o666
  verbose : Bool := false
  deriving Repr, BEq, DecidableEq, Inhabited

structure MkfifoInput where
  args : List String
  deriving Inhabited, BEq

def defaultInput : MkfifoInput := { args := [] }

def parseMode (s : String) : UInt32 :=
  if s.isEmpty then 0o666 else
  let rec go (chars : List Char) (acc : UInt32) : UInt32 :=
    match chars with
    | [] => acc
    | c :: rest =>
      if c ≥ '0' && c ≤ '7' then
        go rest (acc * 8 + (UInt32.ofNat (c.toNat - 0x30)))
      else 0o666
  go (s.toList) 0

def parseArgs (args : List String) : Options × List String :=
  let rec go (remaining : List String) (opts : Options) (names : List String)
      : Options × List String :=
    match remaining with
    | [] => (opts, names.reverse)
    | "--" :: rest => (opts, names.reverse ++ rest)
    | "-m" :: modeStr :: rest => go rest { opts with mode := parseMode modeStr } names
    | "--mode" :: modeStr :: rest => go rest { opts with mode := parseMode modeStr } names
    | "-v" :: rest => go rest { opts with verbose := true } names
    | "--verbose" :: rest => go rest { opts with verbose := true } names
    | s :: rest =>
      if s.startsWith "-" && s != "-" then (opts, names.reverse)
      else go rest opts (s :: names)
  go args {} []

def spec (input : MkfifoInput) : Options × List String := parseArgs input.args

/-- Unfolding lemma: `spec` delegates to `parseArgs`. -/
theorem i_spec_unfold (input : MkfifoInput) : spec input = parseArgs input.args := by
  simp [spec]

theorem i_empty : spec defaultInput = ({}, []) := by native_decide

example : (parseArgs ["fifo"]).2 = ["fifo"] := by native_decide
example : (parseArgs ["-m", "644", "fifo"]).1.mode = 0o644 := by native_decide
example : parseMode "" = 0o666 := by native_decide
example : parseMode "0" = 0 := by native_decide
example : parseMode "644" = 0o644 := by native_decide
example : parseMode "755" = 0o755 := by native_decide
example : parseMode "777" = 0o777 := by native_decide
example : parseMode "8" = 0o666 := by native_decide
example : parseMode "abc" = 0o666 := by native_decide
example : parseMode "0755" = 0o755 := by native_decide
example : (parseArgs ["a", "b", "c"]).2 = ["a", "b", "c"] := by native_decide
example : (parseArgs ["-v", "fifo"]).1.verbose = true := by native_decide

end Lentils.Mkfifo.Logic
