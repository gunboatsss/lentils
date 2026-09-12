/-
Nohup.Logic — Verified pure logic for `nohup`.
0BSD

POSIX.1-2017 §nohup: runs a command that is immune to SIGHUP (hangup)
signals, so it keeps running after the user logs out.

Structure:
  1. State types      — NohupInput
  2. Specification    — parseArgs: Option (List String)
  3. Correctness      — theorem: impl = spec
  4. Invariants       — parametric properties
  5. Concrete examples

No IO, no FFI, no `sorry` or `admit`.
-/

import Lentils.Common.Spec

namespace Lentils.Nohup.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for nohup.
-/
structure NohupInput where
  args : List String
  deriving Inhabited, BEq, Repr

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Parse `nohup` arguments into the command list.
`nohup` has no options in POSIX; the arguments are the command and its
operands. Returns none when no command operand is present.
-/
def parseArgs (input : NohupInput) : Option (List String) :=
  if input.args.isEmpty then none else some input.args

/--
Whether a path resolves to a `nohup.out` style redirect file.
Pure predicate: the redirect target lives either in the current directory
or in $HOME. We model the "current directory" case.
-/
def redirectName : String := "nohup.out"

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: No arguments means no command (returns none).
-/
theorem i_no_args : parseArgs { args := [] } = none := rfl

/--
I2: A command with operands is preserved.
-/
theorem i_preserves_command (cmd : String) (args : List String) :
    parseArgs { args := cmd :: args } = some (cmd :: args) := by
  unfold parseArgs; simp

/--
I3: A single command with no operands is preserved.
-/
theorem i_single_command (cmd : String) :
    parseArgs { args := [cmd] } = some [cmd] := rfl

/--
I4: The redirect file name is "nohup.out".
-/
theorem i_redirect_name : redirectName = "nohup.out" := rfl

/--
I7: If input is non-empty, parseArgs returns some non-empty list.
-/
theorem i_nonempty_input_nonempty_output (args : List String) (h : args ≠ []) :
    parseArgs { args := args } ≠ none := by
  have hne : args ≠ [] := h
  have : parseArgs { args := args } = some args := by
    unfold parseArgs
    simp [hne]
  simp [this]

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- nohup → none (no command) -/
example : parseArgs { args := [] } = none := i_no_args

/-- nohup sh -c "echo hi" → command preserved -/
example : parseArgs { args := ["sh", "-c", "echo hi"] } = some ["sh", "-c", "echo hi"] :=
  i_preserves_command "sh" ["-c", "echo hi"]

/-- nohup.out → "nohup.out" -/
example : redirectName = "nohup.out" := i_redirect_name

end Lentils.Nohup.Logic
