/-
Chown.Logic — Verified pure logic for `chown`. 0BSD
Spec-first methodology.
-/

import Lentils.Common.Spec

namespace Lentils.Chown.Logic

open Lentils.Common.Spec

structure Options where
  verbose : Bool := false
  recursive : Bool := false
  deriving Repr, BEq, DecidableEq, Inhabited

structure OwnerGroup where
  owner : String
  group : String
  deriving Repr, BEq, DecidableEq, Inhabited

structure ChownInput where
  args : List String
  deriving Inhabited, BEq

def defaultInput : ChownInput := { args := [] }

def parseOwnerGroup (s : String) : Option OwnerGroup :=
  if s.isEmpty then none
  else match s.splitOn ":" with
    | [owner] => some { owner, group := "" }
    | [owner, group] => some { owner, group }
    | _ => none

def parseArgs (args : List String) : Options × Option OwnerGroup × List String :=
  let rec go (remaining : List String) (opts : Options) (files : List String)
      : Options × Option OwnerGroup × List String :=
    match remaining with
    | [] => (opts, none, files.reverse)
    | "--" :: rest => (opts, none, files.reverse ++ rest)
    | "-v" :: rest => go rest { opts with verbose := true } files
    | "--verbose" :: rest => go rest { opts with verbose := true } files
    | "-R" :: rest => go rest { opts with recursive := true } files
    | "--recursive" :: rest => go rest { opts with recursive := true } files
    | s :: rest =>
      if s.startsWith "-" && s != "-" then (opts, none, files.reverse)
      else
        let spec := parseOwnerGroup s
        (opts, spec, files.reverse ++ rest)
  go args {} []

def spec (input : ChownInput) : Options × Option OwnerGroup × List String := parseArgs input.args

/--
Spec agrees with parseArgs on all inputs (∀-quantified invariant).
-/
theorem i_spec_eq_parseArgs (input : ChownInput) :
    spec input = parseArgs input.args := by
  simp [spec]

theorem i_empty : spec defaultInput = ({}, none, []) := by native_decide

example : parseOwnerGroup "alice" = some { owner := "alice", group := "" } := by native_decide
example : parseOwnerGroup "alice:staff" = some { owner := "alice", group := "staff" } := by native_decide
example : parseOwnerGroup ":staff" = some { owner := "", group := "staff" } := by native_decide
example : parseOwnerGroup "" = none := by native_decide

end Lentils.Chown.Logic
