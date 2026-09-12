/-
Unlink.Logic — Verified pure logic for `unlink`. 0BSD
Spec-first methodology.
-/

import Lentils.Common.Spec

namespace Lentils.Unlink.Logic

open Lentils.Common.Spec

structure Options where
  deriving Repr, BEq, DecidableEq, Inhabited

structure UnlinkInput where
  args : List String
  deriving Inhabited, BEq

def defaultInput : UnlinkInput := { args := [] }

def parseArgs (args : List String) : Options × List String :=
  let rec go (remaining : List String) (opts : Options) (files : List String)
      : Options × List String :=
    match remaining with
    | [] => (opts, files.reverse)
    | "--" :: rest => (opts, files.reverse ++ rest)
    | s :: rest =>
      if s.startsWith "-" && s != "-" then
        if s == "--help" then go rest opts files
        else (opts, files.reverse)
      else go rest opts (s :: files)
  go args {} []

def spec (input : UnlinkInput) : Options × List String := parseArgs input.args

theorem i_empty : spec defaultInput = ({}, []) := by native_decide

/--
Parsing after `--` returns the remaining args unchanged (parametric over all lists).
-/
theorem parseArgs_double_dash (rest : List String) :
    (parseArgs ("--" :: rest)).2 = rest := by
  unfold parseArgs; rfl

example : (parseArgs ["file"]).2 = ["file"] := by native_decide
example : (parseArgs ["a", "b", "c"]).2 = ["a", "b", "c"] := by native_decide
example : (parseArgs ["--", "-f"]).2 = ["-f"] := by native_decide

end Lentils.Unlink.Logic
