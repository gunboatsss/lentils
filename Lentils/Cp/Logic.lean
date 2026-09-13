/-
Cp.Logic — Verified pure logic for `cp`. 0BSD
Spec-first methodology.
-/

import Lentils.Common.Spec

namespace Lentils.Cp.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

structure Options where
  force : Bool := false
  interactive : Bool := false
  recursive : Bool := false
  verbose : Bool := false
  deriving Repr, BEq, DecidableEq, Inhabited

structure CpInput where
  args : List String
  deriving Inhabited, BEq

def defaultInput : CpInput := { args := [] }

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

def isFlag (s : String) : Bool := s.startsWith "-"

def parseArgs (args : List String) : Options × List String :=
  let rec go (remaining : List String) (opts : Options) (operands : List String)
      : Options × List String :=
    match remaining with
    | [] => (opts, operands.reverse)
    | "--" :: rest => (opts, operands.reverse ++ rest)
    -- GNU last-wins: -f clears -i and vice versa.
    | "-f" :: rest => go rest { opts with force := true, interactive := false } operands
    | "--force" :: rest => go rest { opts with force := true, interactive := false } operands
    | "-i" :: rest => go rest { opts with force := false, interactive := true } operands
    | "--interactive" :: rest => go rest { opts with force := false, interactive := true } operands
    | "-r" :: rest => go rest { opts with recursive := true } operands
    | "-R" :: rest => go rest { opts with recursive := true } operands
    | "--recursive" :: rest => go rest { opts with recursive := true } operands
    | "-v" :: rest => go rest { opts with verbose := true } operands
    | "--verbose" :: rest => go rest { opts with verbose := true } operands
    | s :: rest =>
      if s == "-" then go rest opts (s :: operands)
      else if s.startsWith "-" then
        -- Combined single-char flags (e.g. -rv, -fi with GNU last-wins).
        let chars := s.toList.drop 1
        let rec applyChars (cs : List Char) (o : Options) : Option Options :=
          match cs with
          | [] => some o
          | 'r' :: more => applyChars more { o with recursive := true }
          | 'R' :: more => applyChars more { o with recursive := true }
          | 'v' :: more => applyChars more { o with verbose := true }
          | 'f' :: more => applyChars more { o with force := true, interactive := false }
          | 'i' :: more => applyChars more { o with force := false, interactive := true }
          | _ :: _ => none
        match applyChars chars opts with
        | some o' => go rest o' operands
        | none => (opts, operands.reverse)
      else go rest opts (s :: operands)
  go args {} []

def splitSourcesDest (operands : List String) : List String × Option String :=
  match operands.reverse with
  | [] => ([], none)
  | dest :: revSrcs => (revSrcs.reverse, some dest)

def spec (input : CpInput) : Options × List String × Option String :=
  let (opts, operands) := parseArgs input.args
  let (sources, dest) := splitSourcesDest operands
  (opts, sources, dest)

/--
Spec agrees with parseArgs/splitSourcesDest on all inputs (∀-invariant).
-/
theorem i_spec_eq_parse (input : CpInput) :
    spec input =
      let (opts, operands) := parseArgs input.args
      let (sources, dest) := splitSourcesDest operands
      (opts, sources, dest) := by
  simp [spec]

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Invariants
-- ═══════════════════════════════════════════════════════════════════════════════════

theorem i_empty : spec defaultInput = ({}, [], none) := by
  native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

example : (parseArgs ["-r", "a", "b"]).1.recursive = true := by native_decide
example : (parseArgs ["--recursive", "-v", "a", "b"]).1 =
  { force := false, recursive := true, verbose := true } := by native_decide
example : (parseArgs ["--", "-r"]).2 = ["-r"] := by native_decide
example : splitSourcesDest ["a", "b"] = (["a"], some "b") := by native_decide
example : splitSourcesDest ["a"] = ([], some "a") := by native_decide
example : splitSourcesDest [] = ([], none) := by native_decide

end Lentils.Cp.Logic
