/-
Mkdir.Logic — Verified pure logic for `mkdir`. 0BSD
Spec-first methodology.
-/

import Lentils.Common.Spec

namespace Lentils.Mkdir.Logic

open Lentils.Common.Spec

structure Options where
  parents : Bool := false
  deriving Repr, BEq, DecidableEq, Inhabited

structure MkdirInput where
  args : List String
  deriving Inhabited, BEq

def defaultInput : MkdirInput := { args := [] }

def isFlag (s : String) : Bool := s.startsWith "-"

def parseArgs (args : List String) : Options × List String :=
  let rec go (remaining : List String) (opts : Options) (paths : List String)
      : Options × List String :=
    match remaining with
    | [] => (opts, paths.reverse)
    | "--" :: rest => (opts, (paths.reverse ++ rest))
    | "-p" :: rest => go rest { opts with parents := true } paths
    | "--parents" :: rest => go rest { opts with parents := true } paths
    | s :: rest =>
      if s.startsWith "-" then (opts, paths.reverse)
      else go rest opts (s :: paths)
  go args {} []

def spec (input : MkdirInput) : Options × List String := parseArgs input.args

/-- Unfolding lemma: `spec` delegates to `parseArgs`. -/
theorem i_spec_unfold (input : MkdirInput) : spec input = parseArgs input.args := by
  simp [spec]

theorem i_empty : spec defaultInput = ({}, []) := by native_decide

example : (parseArgs []).1.parents = false := by native_decide
example : (parseArgs ["-p", "dir"]).1.parents = true := by native_decide
example : (parseArgs ["--parents", "dir"]).1.parents = true := by native_decide
example : (parseArgs ["somedir"]).2 = ["somedir"] := by native_decide
example : (parseArgs ["--", "-p"]).2 = ["-p"] := by native_decide

end Lentils.Mkdir.Logic
