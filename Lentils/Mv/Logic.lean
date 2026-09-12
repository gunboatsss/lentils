/-
Mv.Logic — Verified pure logic for `mv`. 0BSD
Spec-first methodology.
-/

import Lentils.Common.Spec

namespace Lentils.Mv.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

structure Options where
  force : Bool := false
  interactive : Bool := false
  noClobber : Bool := false
  verbose : Bool := false
  deriving Repr, BEq, DecidableEq, Inhabited

structure MvInput where
  args : List String
  deriving Inhabited, BEq

def defaultInput : MvInput := { args := [] }

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
    | "-f" :: rest => go rest { opts with force := true } operands
    | "--force" :: rest => go rest { opts with force := true } operands
    | "-i" :: rest => go rest { opts with interactive := true } operands
    | "--interactive" :: rest => go rest { opts with interactive := true } operands
    | "-n" :: rest => go rest { opts with noClobber := true } operands
    | "--no-clobber" :: rest => go rest { opts with noClobber := true } operands
    | "-v" :: rest => go rest { opts with verbose := true } operands
    | "--verbose" :: rest => go rest { opts with verbose := true } operands
    | s :: rest =>
      if s.startsWith "-" then (opts, operands.reverse)
      else go rest opts (s :: operands)
  go args {} []

def splitSourcesDest (operands : List String) : List String × Option String :=
  match operands.reverse with
  | [] => ([], none)
  | dest :: revSrcs => (revSrcs.reverse, some dest)

def targetPath (dest : String) (source : String) (destIsDir : Bool) : String :=
  if destIsDir then
    (System.FilePath.mk dest / source).toString
  else
    dest

def spec (input : MvInput) : Options × List String × Option String :=
  let (opts, operands) := parseArgs input.args
  let (sources, dest) := splitSourcesDest operands
  (opts, sources, dest)

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Invariants
-- ═══════════════════════════════════════════════════════════════════════════════════

theorem i_empty : spec defaultInput = ({}, [], none) := by native_decide

theorem i_target_plain (dest source : String) : targetPath dest source false = dest := by
  simp [targetPath]

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

example : targetPath "dir" "s.txt" true = "dir/s.txt" := by native_decide
example : (parseArgs ["-v", "a", "b"]).1.verbose = true := by native_decide
example : (parseArgs ["-i", "a", "b"]).1.interactive = true := by native_decide
example : (parseArgs ["--", "-v"]).2 = ["-v"] := by native_decide
example : splitSourcesDest ["a", "b"] = (["a"], some "b") := by native_decide
example : splitSourcesDest [] = ([], none) := by native_decide

end Lentils.Mv.Logic
