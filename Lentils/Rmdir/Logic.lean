/-
Rmdir.Logic — Verified pure logic for `rmdir`. 0BSD
Spec-first methodology.
-/

import Lentils.Common.Spec

namespace Lentils.Rmdir.Logic

open Lentils.Common.Spec

structure Options where
  parents : Bool := false
  verbose : Bool := false
  deriving Repr, BEq, DecidableEq, Inhabited

structure RmdirInput where
  args : List String
  deriving Inhabited, BEq

def defaultInput : RmdirInput := { args := [] }

def isFlag (s : String) : Bool := s.startsWith "-"

def applyShort (c : Char) (opts : Options) : Option Options :=
  match c with
  | 'p' => some { opts with parents := true }
  | 'v' => some { opts with verbose := true }
  | _ => none

def applyShortFlags (s : String) (opts : Options) : Option Options :=
  let chars := s.toList.tail
  let rec go (cs : List Char) (o : Options) : Option Options :=
    match cs with
    | [] => some o
    | c :: r =>
      match applyShort c o with
      | none => none
      | some o2 => go r o2
  go chars opts

def parseArgs (args : List String) : Options × List String :=
  let rec go (remaining : List String) (opts : Options) (operands : List String)
      : Options × List String :=
    match remaining with
    | [] => (opts, operands.reverse)
    | "--" :: rest => (opts, operands.reverse ++ rest)
    | "-p" :: rest => go rest { opts with parents := true } operands
    | "--parents" :: rest => go rest { opts with parents := true } operands
    | "-v" :: rest => go rest { opts with verbose := true } operands
    | "--verbose" :: rest => go rest { opts with verbose := true } operands
    | s :: rest =>
      if s == "-" then go rest opts (s :: operands)
      else if s.startsWith "-" then
        if s.length >= 3 then
          match applyShortFlags s opts with
          | none => (opts, operands.reverse)
          | some newOpts => go rest newOpts operands
        else (opts, operands.reverse)
      else go rest opts (s :: operands)
  go args {} []

def spec (input : RmdirInput) : Options × List String := parseArgs input.args

theorem i_empty : spec defaultInput = ({}, []) := by native_decide

/--
Short flag 'p' sets parents, leaving other options untouched.
Parametric over all option states.
-/
theorem i_applyShort_parents (opts : Options) :
    applyShort 'p' opts = some { opts with parents := true } := by
  simp [applyShort]

/--
Short flag 'v' sets verbose, leaving other options untouched.
Parametric over all option states.
-/
theorem i_applyShort_verbose (opts : Options) :
    applyShort 'v' opts = some { opts with verbose := true } := by
  simp [applyShort]

example : (parseArgs ["dir"]).2 = ["dir"] := by native_decide
example : (parseArgs ["-p", "dir"]).1.parents = true := by native_decide
example : (parseArgs ["--parents", "-v", "dir"]).1 =
  { parents := true, verbose := true } := by native_decide
example : (parseArgs ["--", "-p"]).2 = ["-p"] := by native_decide

end Lentils.Rmdir.Logic
