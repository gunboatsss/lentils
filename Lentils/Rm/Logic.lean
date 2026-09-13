/-
Rm.Logic — Verified pure logic for `rm`. 0BSD
Spec-first methodology.
-/

import Lentils.Common.Spec

namespace Lentils.Rm.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

structure Options where
  force : Bool := false
  interactive : Bool := false
  recursive : Bool := false
  dir : Bool := false
  verbose : Bool := false
  deriving Repr, BEq, DecidableEq, Inhabited

structure RmInput where
  args : List String
  deriving Inhabited, BEq

def defaultInput : RmInput := { args := [] }

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

def isFlag (s : String) : Bool := s.startsWith "-"

def applyShort (c : Char) (opts : Options) : Option Options :=
  match c with
  -- GNU last-wins: -f clears -i and vice versa.
  | 'f' => some { opts with force := true, interactive := false }
  | 'i' => some { opts with force := false, interactive := true }
  | 'r' => some { opts with recursive := true }
  | 'R' => some { opts with recursive := true }
  | 'd' => some { opts with dir := true }
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
    | "-f" :: rest => go rest { opts with force := true, interactive := false } operands
    | "--force" :: rest => go rest { opts with force := true, interactive := false } operands
    | "-i" :: rest => go rest { opts with force := false, interactive := true } operands
    | "--interactive" :: rest => go rest { opts with force := false, interactive := true } operands
    | "-r" :: rest => go rest { opts with recursive := true } operands
    | "-R" :: rest => go rest { opts with recursive := true } operands
    | "--recursive" :: rest => go rest { opts with recursive := true } operands
    | "-d" :: rest => go rest { opts with dir := true } operands
    | "--dir" :: rest => go rest { opts with dir := true } operands
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

def spec (input : RmInput) : Options × List String := parseArgs input.args

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Invariants
-- ═══════════════════════════════════════════════════════════════════════════════════

theorem i_empty : spec defaultInput = ({}, []) := by native_decide

/--
Short flag 'f' sets force and clears interactive (GNU last-wins),
leaving other options untouched. Parametric over all option states.
-/
theorem i_applyShort_force (opts : Options) :
    applyShort 'f' opts = some { opts with force := true, interactive := false } := by
  simp [applyShort]

/--
Short flag 'r' sets recursive, leaving other options untouched.
Parametric over all option states.
-/
theorem i_applyShort_recursive (opts : Options) :
    applyShort 'r' opts = some { opts with recursive := true } := by
  simp [applyShort]

theorem i_parse_recursive_force : (parseArgs ["--recursive", "-f", "file"]).1 =
    { force := true, recursive := true, interactive := false, dir := false, verbose := false } := by
  native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

example : (parseArgs []).2 = [] := by native_decide
example : (parseArgs ["-d", "dir"]).1.dir = true := by native_decide
example : (parseArgs ["--", "-r"]).2 = ["-r"] := by native_decide
example : (parseArgs ["-rf", "file"]).1 =
  { force := true, interactive := false, recursive := true, dir := false, verbose := false } := by native_decide

end Lentils.Rm.Logic
