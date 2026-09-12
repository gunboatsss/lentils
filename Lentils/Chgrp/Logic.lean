/-
Chgrp.Logic — Verified pure logic for `chgrp`. 0BSD
Spec-first methodology.
-/

import Lentils.Common.Spec

namespace Lentils.Chgrp.Logic

open Lentils.Common.Spec

structure Options where
  verbose : Bool := false
  recursive : Bool := false
  deriving Repr, BEq, DecidableEq, Inhabited

structure ParsedArgs where
  options : Options
  group : String
  files : List String
  deriving Repr, BEq, DecidableEq, Inhabited

structure ChgrpInput where
  args : List String
  deriving Inhabited, BEq

def defaultInput : ChgrpInput := { args := [] }

def parseArgs (args : List String) : ParsedArgs :=
  let rec go (remaining : List String) (opts : Options) (files : List String) : ParsedArgs :=
    match remaining with
    | [] => { options := opts, group := "", files := files.reverse }
    | "--" :: rest => { options := opts, group := "", files := files.reverse ++ rest }
    | "-v" :: rest => go rest { opts with verbose := true } files
    | "--verbose" :: rest => go rest { opts with verbose := true } files
    | "-R" :: rest => go rest { opts with recursive := true } files
    | "--recursive" :: rest => go rest { opts with recursive := true } files
    | s :: rest =>
      if s.startsWith "-" && s != "-" then
        { options := opts, group := "", files := files.reverse }
      else
        { options := opts, group := s, files := files.reverse ++ rest }
  go args {} []

def spec (input : ChgrpInput) : ParsedArgs := parseArgs input.args

/--
Spec agrees with parseArgs on all inputs (∀-quantified invariant).
-/
theorem i_spec_eq_parseArgs (input : ChgrpInput) :
    spec input = parseArgs input.args := by
  simp [spec]

theorem i_empty : spec defaultInput = { options := {}, group := "", files := [] } := by
  native_decide

example : (parseArgs ["staff", "file"]).group = "staff" := by native_decide
example : (parseArgs ["-v", "staff", "file"]).options.verbose = true := by native_decide
example : (parseArgs ["-R", "staff", "file"]).options.recursive = true := by native_decide
example : (parseArgs []).group = "" := by native_decide
example : (parseArgs ["staff", "a", "b", "c"]).files = ["a", "b", "c"] := by native_decide
example : (parseArgs ["--", "-v"]).group = "" := by native_decide
example : (parseArgs ["-x", "staff"]).group = "" := by native_decide

end Lentils.Chgrp.Logic
