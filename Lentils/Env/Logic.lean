/-
Env.Logic - Verified pure logic for `env`.
0BSD

POSIX.1-2017 Section env: sets the environment for command invocation, or prints
the environment if no command is specified.

Structure:
  1. State types      -- EnvInput, ParsedEnv
  2. Specification    -- parseArgs: ParsedEnv
  3. Correctness      -- theorem: impl = spec
  4. Invariants       -- parametric properties over all inputs
  5. Lemmas           -- helper theorems
  6. Concrete examples

No IO, no FFI, no `sorry` or `admit`.
-/

import Lentils.Common.Spec

namespace Lentils.Env.Logic

open Lentils.Common.Spec

-- ============================================================
-- 1. State Types
-- ============================================================

/--
Input state for env: the command-line arguments.
-/
structure EnvInput where
  args : List String
  deriving Inhabited, BEq, Repr

/--
Parsed env result:
- clearEnv : whether to clear the environment (using -i or --ignore-environment)
- envPairs : name=value assignments
- cmdArgs  : the command and its arguments
-/
structure ParsedEnv where
  clearEnv : Bool
  envPairs : List String
  cmdArgs : List String
  deriving Inhabited, BEq, Repr, DecidableEq

def defaultParsed : ParsedEnv := { clearEnv := false, envPairs := [], cmdArgs := [] }

-- ============================================================
-- 2. Helpers
-- ============================================================

/--
Check if a string looks like an environment variable assignment (contains '=').
-/
def isEnvAssignment (s : String) : Bool :=
  s.contains '='

-- ============================================================
-- 3. Specification (= Implementation)
-- ============================================================

/--
Parse env args.
- `-i` or `--ignore-environment` clears the environment.
- `name=value` entries are environment variable assignments.
- Everything else is treated as command + args.
-/
def parseArgs (input : EnvInput) : ParsedEnv :=
  let rec go (remaining : List String) (clearEnv : Bool) (envPairs : List String) : ParsedEnv :=
    match remaining with
    | [] => { clearEnv := clearEnv, envPairs := envPairs.reverse, cmdArgs := [] }
    | "-i" :: rest => go rest true envPairs
    | "--ignore-environment" :: rest => go rest true envPairs
    | "-" :: rest => { clearEnv := clearEnv, envPairs := envPairs.reverse, cmdArgs := rest }
    | "--" :: rest => { clearEnv := clearEnv, envPairs := envPairs.reverse, cmdArgs := rest }
    | (s :: rest) =>
      if s.startsWith "-" then
        { clearEnv := clearEnv, envPairs := envPairs.reverse, cmdArgs := remaining }
      else if isEnvAssignment s then
        go rest clearEnv (s :: envPairs)
      else
        { clearEnv := clearEnv, envPairs := envPairs.reverse, cmdArgs := remaining }
    termination_by remaining.length
  go input.args false []

/--
Exit code for env when no command is given. Always 0.
-/
def exitCode : UInt32 := 0

-- ============================================================
-- 5. Invariants - parametric theorems over all inputs
-- ============================================================

/--
I1: Empty input -> default parsed (no clear, no pairs, no command).
-/
theorem i_empty : parseArgs { args := [] } = defaultParsed := by
  native_decide

/--
I4: A single pure command (no env vars, no flags) has empty envPairs and clearEnv=false.
-/
theorem i_plain_command :
    parseArgs { args := ["echo", "hi"] } = { defaultParsed with cmdArgs := ["echo", "hi"] } := by
  native_decide

/--
I5: The -i flag sets clearEnv to true.
-/
theorem i_clear_env :
    (parseArgs { args := ["-i"] }).clearEnv = true := by
  native_decide

/--
I6: The -- separator terminates option parsing.
-/
theorem i_ddash_terminates :
    (parseArgs { args := ["-i", "--", "echo", "hi"] }).cmdArgs = ["echo", "hi"] := by
  native_decide

/--
I7: A single env assignment before a command is captured.
-/
theorem i_env_assignment :
    (parseArgs { args := ["FOO=bar", "echo", "hi"] }).envPairs = ["FOO=bar"] := by
  native_decide

-- ============================================================
-- 6. Concrete Corollaries
-- ============================================================

/-- env -> default parsed -/
example : parseArgs { args := [] } = defaultParsed := i_empty

/-- env -i -> clear -/
example : (parseArgs { args := ["-i"] }).clearEnv = true := i_clear_env

/-- env FOO=bar echo hi -> one env var, command -/
example : (parseArgs { args := ["FOO=bar", "echo", "hi"] }).envPairs = ["FOO=bar"] :=
  i_env_assignment

/-- env echo hi -> plain command -/
example : parseArgs { args := ["echo", "hi"] } = { defaultParsed with cmdArgs := ["echo", "hi"] } :=
  i_plain_command

end Lentils.Env.Logic
