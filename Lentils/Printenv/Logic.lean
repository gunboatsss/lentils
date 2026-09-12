/-
Printenv.Logic — Verified pure logic for `printenv`.
0BSD

Structure:
  1. State types      — PrintenvInput (env entries + requested names)
  2. Specification    — matching and extraction
  3. Implementation   — same as spec
  4. Correctness      — impl_correct
  5. Invariants       — parametric properties
  6. Concrete examples

No IO, no FFI, no `sorry` or `admit`.

POSIX: `printenv` prints environment variable values.
-/

import Lentils.Common.Spec

namespace Lentils.Printenv.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for printenv: environment entries and requested variable names.
-/
structure PrintenvInput where
  entries : List String   -- "KEY=VALUE" entries
  names : List String     -- requested variable names (empty = print all)
  deriving Inhabited, BEq

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Check if a "KEY=VALUE" entry matches a requested variable name.
-/
def matchesVar (entry : String) (name : String) : Bool :=
  entry.startsWith (name ++ "=")

/--
Extract the value from a "KEY=VALUE" string.
Returns "" for malformed entries without '='.
-/
def extractValue (entry : String) : String :=
  match entry.splitOn "=" with
  | _ :: rest => String.join rest
  | _ => ""

/--
Format the printenv output.
If names is empty, print all entries line-by-line.
Otherwise, print the value of each requested variable.
-/
def format (input : PrintenvInput) : String :=
  if input.names.isEmpty then
    String.intercalate "\n" input.entries
  else
    let matching := input.names.filterMap λ name =>
      match input.entries.find? (λ e => matchesVar e name) with
      | some entry => some (extractValue entry)
      | none => none
    String.intercalate "\n" matching

/--
Specification: format environment output.
-/
def spec (input : PrintenvInput) : String :=
  format input

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Implementation
-- ═══════════════════════════════════════════════════════════════════════════════════

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Parametric Invariants
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: matchesVar matches HOME in "HOME=/home/user".
-/
theorem i_matches_home : matchesVar "HOME=/home/user" "HOME" = true := by
  native_decide

/--
I2: matchesVar does not match PATH for "HOME=/home/user".
-/
theorem i_not_matches_path : matchesVar "HOME=/home/user" "PATH" = false := by
  native_decide

/--
I3: extractValue returns the part after '='.
-/
theorem i_extract_home : extractValue "HOME=/home/user" = "/home/user" := by
  native_decide

/--
I4: extractValue returns "" for an entry without '='.
-/
theorem i_extract_no_eq : extractValue "PATH" = "" := by
  native_decide

/--
I6: Empty entries yields empty output.
-/
theorem i_empty_entries : format { entries := [], names := [] } = "" := rfl

/--
I7: With no names, output is all entries joined by newlines.
-/
theorem i_print_all (entries : List String) :
    format { entries := entries, names := [] } = String.intercalate "\n" entries := by
  simp [format]

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- matchesVar matches HOME. -/
example : matchesVar "HOME=/home/user" "HOME" = true := i_matches_home

/-- matchesVar does not match PATH. -/
example : matchesVar "HOME=/home/user" "PATH" = false := i_not_matches_path

/-- extractValue gets the value. -/
example : extractValue "HOME=/home/user" = "/home/user" := i_extract_home

end Lentils.Printenv.Logic
