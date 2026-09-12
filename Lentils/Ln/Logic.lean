/-
Ln.Logic — Verified pure logic for `ln`. 0BSD
Spec-first methodology.
-/

import Lentils.Common.Spec

namespace Lentils.Ln.Logic

open Lentils.Common.Spec

structure Options where
  symbolic : Bool := false
  force : Bool := false
  verbose : Bool := false
  deriving Repr, BEq, DecidableEq, Inhabited

structure LnInput where
  args : List String
  deriving Inhabited, BEq

def defaultInput : LnInput := { args := [] }

def isFlag (s : String) : Bool := s.startsWith "-"

def applyShort (c : Char) (opts : Options) : Option Options :=
  match c with
  | 's' => some { opts with symbolic := true }
  | 'f' => some { opts with force := true }
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
    | "-s" :: rest => go rest { opts with symbolic := true } operands
    | "--symbolic" :: rest => go rest { opts with symbolic := true } operands
    | "-f" :: rest => go rest { opts with force := true } operands
    | "--force" :: rest => go rest { opts with force := true } operands
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

def splitSourcesLink (operands : List String) : List String × Option String :=
  match operands.reverse with
  | [] => ([], none)
  | [only] => ([only], none)
  | link :: revSrcs => (revSrcs.reverse, some link)

def spec (input : LnInput) : Options × List String × Option String :=
  let (opts, operands) := parseArgs input.args
  let (sources, linkName) := splitSourcesLink operands
  (opts, sources, linkName)

theorem i_empty : spec defaultInput = ({}, [], none) := by native_decide

theorem i_splitSourcesLink_single (s : String) :
    splitSourcesLink [s] = ([s], none) := by
  unfold splitSourcesLink
  simp

example : splitSourcesLink ["file"] = (["file"], none) := by native_decide
example : splitSourcesLink ["a", "b"] = (["a"], some "b") := by native_decide
example : (parseArgs ["-s", "a", "b"]).1.symbolic = true := by native_decide
example : (parseArgs ["-sf", "a", "b"]).1 =
  { symbolic := true, force := true, verbose := false } := by native_decide
example : (parseArgs ["--symbolic", "-f", "a", "b"]).1 =
  { symbolic := true, force := true, verbose := false } := by native_decide

end Lentils.Ln.Logic
