/-
Pathchk.Logic — Verified pure logic for `pathchk`. 0BSD
Spec-first methodology.
-/

import Lentils.Common.Spec

namespace Lentils.Pathchk.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

structure Options where
  portable : Bool := false
  strict : Bool := false
  deriving Repr, BEq, DecidableEq, Inhabited

structure PathchkInput where
  args : List String
  deriving Inhabited, BEq

def defaultInput : PathchkInput := { args := [] }

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

def maxComponentLen : Nat := 255
def maxPathLen : Nat := 4096
def portableMaxComponentLen : Nat := 14
def portableMaxPathLen : Nat := 256

def portableChars : List Char :=
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._-".toList

def isPortableChar (c : Char) : Bool :=
  portableChars.any (λ p => p == c)

def enumerate (xs : List String) : List (Nat × String) :=
  let rec go (i : Nat) (remaining : List String) : List (Nat × String) :=
    match remaining with
    | [] => []
    | x :: xs => (i, x) :: go (i + 1) xs
  go 0 xs

def checkComponent (comp : String) (portable : Bool) (isFirst : Bool) (strict : Bool := false) : Option String :=
  if comp.contains (λ c => c == '\x00') then some "null byte in component"
  else if comp.isEmpty then
    if isFirst then none
    else if strict then some "empty component"
    else none
  else if strict && comp.startsWith "-" then some "leading dash"
  else if portable && comp.any (λ c => !isPortableChar c) then some "non-portable character"
  else
    let maxLen := if portable then portableMaxComponentLen else maxComponentLen
    if comp.length > maxLen then some s!"component too long ({comp.length} > {maxLen})"
    else none

def checkPath (path : String) (portable : Bool) (strict : Bool := false) : List String :=
  if path.contains (λ c => c == '\x00') then ["null byte in path"]
  else
    let maxPLen := if portable then portableMaxPathLen else maxPathLen
    let errs := if path.length > maxPLen then [s!"path too long ({path.length} > {maxPLen})"] else []
    let components := path.splitOn "/"
    let indexed := enumerate components
    let compErrors := List.filterMap (λ (i, comp) => checkComponent comp portable (i == 0) strict) indexed
    errs ++ compErrors

def formatErrors (path : String) (errors : List String) : String :=
  String.intercalate "\n" (errors.map (λ e => s!"{path}: {e}"))

def runCheck (path : String) (portable : Bool) (strict : Bool := false) : String :=
  formatErrors path (checkPath path portable strict)

def isValidPath (path : String) (portable : Bool) (strict : Bool := false) : Bool :=
  (checkPath path portable strict).isEmpty

def spec (input : PathchkInput) : List String :=
  List.flatten (input.args.map (λ p => checkPath p false false))

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Invariants
-- ═══════════════════════════════════════════════════════════════════════════════════

theorem i_empty : spec defaultInput = [] := by
  native_decide

/-- Unfolding lemma: `spec` checks every operand path. -/
theorem i_spec_unfold (input : PathchkInput) :
    spec input = List.flatten (input.args.map (λ p => checkPath p false false)) := by
  simp [spec]

theorem i_portable_char_space : isPortableChar ' ' = false := rfl
theorem i_portable_char_hyphen : isPortableChar '-' = true := rfl
theorem i_portable_char_underscore : isPortableChar '_' = true := rfl
theorem i_portable_char_dot : isPortableChar '.' = true := rfl

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

example : checkComponent "" false false true = some "empty component" := by native_decide
example : checkComponent "" false false false = none := by native_decide
example : checkComponent "" false true = none := by native_decide
example : checkComponent "a" false false = none := by native_decide
example : checkComponent "a" true true = none := by native_decide
example : checkPath "/usr/bin/ls" false = [] := by native_decide

end Lentils.Pathchk.Logic
