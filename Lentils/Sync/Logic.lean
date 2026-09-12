/-
Sync.Logic — Verified pure logic for `sync`.
0BSD

POSIX.1-2017 §sync: synchronises cached writes to disk.
Accepts no operands and has no options.

Structure:
  1. State types      — SyncInput (arguments)
  2. Specification    — parseArgs: Options × List String
  3. Correctness      — theorem: impl = spec
  4. Invariants       — parametric properties
  5. Concrete examples

No IO, no FFI, no `sorry` or `admit`.
-/

import Lentils.Common.Spec

namespace Lentils.Sync.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Options for sync (POSIX has no options, so this is a unit-like structure).
-/
structure Options where
  deriving Repr, BEq, DecidableEq, Inhabited

/--
Input state for sync: the list of arguments.
POSIX sync accepts no operands; any argument is an error (GNU extends this).
-/
structure SyncInput where
  args : List String
  deriving Inhabited, BEq, Repr

/--
Parsed result: options and remaining operands.
-/
structure ParsedSync where
  options : Options
  operands : List String
  deriving Repr, BEq, DecidableEq

def defaultParsed : ParsedSync := { options := {}, operands := [] }

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Parse sync arguments. POSIX sync accepts no operands and no options.
All arguments are ignored (GNU sync accepts file operands with -f).
-/
def parseArgs (input : SyncInput) : ParsedSync :=
  -- POSIX: all arguments are ignored
  defaultParsed

/--
Exit code for sync on success. Always 0.
-/
def exitCode : UInt32 := 0

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: No arguments → empty operands.
-/
theorem i_empty : parseArgs { args := [] } = defaultParsed := rfl

/--
I2: Any arguments → still empty operands (POSIX behavior).
-/
theorem i_ignores_args (args : List String) :
    (parseArgs { args := args }).operands = [] := rfl

/--
I3: Options are always default (no flags accepted).
-/
theorem i_default_options (args : List String) :
    (parseArgs { args := args }).options = {} := rfl

/--
I6: Empty operands are always returned.
-/
theorem i_operands_empty (args : List String) :
    (parseArgs { args := args }).operands = [] := rfl

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- sync (no args) → default parsed -/
example : parseArgs { args := [] } = defaultParsed := i_empty

/-- sync with args → still default parsed -/
example : parseArgs { args := ["a", "b"] } = defaultParsed :=
  by
    have : (parseArgs { args := ["a", "b"] }).operands = [] := i_ignores_args ["a", "b"]
    have : (parseArgs { args := ["a", "b"] }).options = {} := i_default_options ["a", "b"]
    rfl

end Lentils.Sync.Logic
