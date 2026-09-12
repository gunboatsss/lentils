/-
Touch.Logic — Verified pure logic for `touch`. 0BSD
Spec-first methodology.
-/

import Lentils.Common.Spec

namespace Lentils.Touch.Logic

open Lentils.Common.Spec

structure Options where
  noCreate : Bool := false
  force : Bool := false
  deriving Repr, BEq, DecidableEq, Inhabited

structure TouchInput where
  args : List String
  deriving Inhabited, BEq

def defaultInput : TouchInput := { args := [] }

def parseArgs (args : List String) : Options × List String :=
  let rec go (remaining : List String) (opts : Options) (operands : List String)
      : Options × List String :=
    match remaining with
    | [] => (opts, operands.reverse)
    | "--" :: rest => (opts, operands.reverse ++ rest)
    | "-c" :: rest => go rest { opts with noCreate := true } operands
    | "--no-create" :: rest => go rest { opts with noCreate := true } operands
    | "-f" :: rest => go rest { opts with force := true } operands
    | "--force" :: rest => go rest { opts with force := true } operands
    | s :: rest =>
      if s.startsWith "-" then (opts, operands.reverse)
      else go rest opts (s :: operands)
  go args {} []

def spec (input : TouchInput) : Options × List String := parseArgs input.args

theorem i_empty : spec defaultInput = ({}, []) := by native_decide

/--
Parsing after `--` returns the remaining args unchanged (parametric over all lists).
-/
theorem parseArgs_double_dash (rest : List String) :
    (parseArgs ("--" :: rest)).2 = rest := by
  unfold parseArgs; rfl

example : (parseArgs ["file"]).2 = ["file"] := by native_decide
example : (parseArgs ["-c", "file"]).1.noCreate = true := by native_decide
example : (parseArgs ["--no-create", "file"]).1.noCreate = true := by native_decide
example : (parseArgs ["--", "-c"]).2 = ["-c"] := by native_decide

end Lentils.Touch.Logic
