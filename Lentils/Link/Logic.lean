/-
Link.Logic — Verified pure logic for `link`. 0BSD
Spec-first methodology.
-/

import Lentils.Common.Spec

namespace Lentils.Link.Logic

open Lentils.Common.Spec

structure Options where
  deriving Repr, BEq, DecidableEq, Inhabited

structure LinkInput where
  args : List String
  deriving Inhabited, BEq

def defaultInput : LinkInput := { args := [] }

def parseArgs (args : List String) : Options × List String :=
  let rec go (remaining : List String) (opts : Options) (operands : List String)
      : Options × List String :=
    match remaining with
    | [] => (opts, operands.reverse)
    | "--" :: rest => (opts, operands.reverse ++ rest)
    | s :: rest =>
      if s.startsWith "-" && s != "-" then (opts, operands.reverse)
      else go rest opts (s :: operands)
  go args {} []

def spec (input : LinkInput) : Options × List String := parseArgs input.args

theorem i_empty : spec defaultInput = ({}, []) := by native_decide

theorem i_parseArgs_of_nil (args : List String) (h : args = []) :
    parseArgs args = ({}, []) := by
  subst h
  unfold parseArgs
  rfl

example : (parseArgs ["old", "new"]).2 = ["old", "new"] := by native_decide
example : (parseArgs ["--", "old", "new"]).2 = ["old", "new"] := by native_decide
example : (parseArgs []).2 = [] := by native_decide
example : (parseArgs ["only"]).2 = ["only"] := by native_decide
example : (parseArgs ["a", "b", "c"]).2 = ["a", "b", "c"] := by native_decide
example : (parseArgs ["a", "-x"]).2 = ["a"] := by native_decide
example : (parseArgs ["--", "-x"]).2 = ["-x"] := by native_decide

end Lentils.Link.Logic
