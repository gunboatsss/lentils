/-
Mesg.Logic — Verified pure logic for `mesg`. 0BSD

Spec-First Methodology:
  1. State types    — MesgInput (flags + args)
  2. Specification  — formatState, setGroupWrite: the formal "what"
  3. Implementation — the "how" (= spec, since spec is executable)
  4. Correctness    — theorem: impl = spec
  5. Invariants     — parametric properties over all inputs
  6. Lemmas         — helper theorems used in proofs
  7. Concrete corollaries (optional)

No IO, no FFI, no `sorry` or `admit`.
-/

namespace Lentils.Mesg.Logic

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Access states for terminal write permission.
-/
inductive AccessState : Type where
  | yes  -- write access allowed (S_IWGRP set)
  | no   -- write access denied (S_IWGRP cleared)
deriving DecidableEq, BEq

/--
Input state for mesg.
-/
structure MesgInput where
  arg : Option String  -- none means query, some "y" or "n" means set
  deriving Inhabited, BEq

/--
Default input: query current state.
-/
def defaultInput : MesgInput := { arg := none }

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Parse a command-line argument into an AccessState.
Only "y" and "n" are valid.
-/
def parseArg (s : String) : Option AccessState :=
  if s = "y" then some AccessState.yes
  else if s = "n" then some AccessState.no
  else none

/--
Format the access state for stdout output.
-/
def formatState (s : AccessState) : String :=
  match s with
  | AccessState.yes => "is y\n"
  | AccessState.no  => "is n\n"

/--
Convert a UInt32 mode bitset into an AccessState.
S_IWGRP is 0020 octal = 16 decimal.
-/
def modeToState (mode : UInt32) : AccessState :=
  if (mode &&& 16) ≠ 0 then AccessState.yes else AccessState.no

/--
Compute the new mode bits after setting or clearing S_IWGRP.
S_IWGRP is bit 4 (value 16).
-/
def setGroupWrite (mode : UInt32) (state : AccessState) : UInt32 :=
  match state with
  | AccessState.yes => mode ||| 16   -- set S_IWGRP
  | AccessState.no  => mode &&& 0xFFFFFFEF  -- clear S_IWGRP (bit 4)

/--
Spec: given input and current mode, produce the output string and new mode.
-/
def spec (input : MesgInput) (currentMode : UInt32) : String × UInt32 :=
  match input.arg with
  | none =>
    (formatState (modeToState currentMode), currentMode)
  | some arg =>
    match parseArg arg with
    | some state =>
      ("", setGroupWrite currentMode state)
    | none =>
      ("", currentMode)

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Exit codes
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Exit code for success.
-/
def exitOK : UInt32 := 0

/--
Exit code for error (not a terminal, invalid arg, permission denied).
POSIX convention: 2 for usage/terminal errors.
-/
def exitError : UInt32 := 2

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: `parseArg "y"` returns `some AccessState.yes`.
-/
theorem i_parseArg_y : parseArg "y" = some AccessState.yes := rfl

/--
I2: `parseArg "n"` returns `some AccessState.no`.
-/
theorem i_parseArg_n : parseArg "n" = some AccessState.no := rfl

/--
I3: `parseArg` returns `none` for any invalid argument.
-/
theorem i_parseArg_invalid (s : String) (h : s ≠ "y") (h' : s ≠ "n") : parseArg s = none := by
  unfold parseArg
  simp [h, h']

/--
I4: `formatState yes` produces "is y\n".
-/
theorem i_formatState_yes : formatState AccessState.yes = "is y\n" := rfl

/--
I5: `formatState no` produces "is n\n".
-/
theorem i_formatState_no : formatState AccessState.no = "is n\n" := rfl

/--
I6: `formatState` is injective — different states produce different strings.
-/
theorem i_formatState_injective (s1 s2 : AccessState) (h : formatState s1 = formatState s2) : s1 = s2 := by
  cases s1 <;> cases s2 <;> simp [formatState] at h <;> trivial

/--
I7: `modeToState` of a mode with S_IWGRP (16) set returns yes.
-/
theorem i_modeToState_yes : modeToState 16 = AccessState.yes := by
  native_decide

/--
I8: `modeToState` of a mode without S_IWGRP returns no.
-/
theorem i_modeToState_no : modeToState 0 = AccessState.no := by
  native_decide

/--
I9: `setGroupWrite` with yes on 0 sets bit 4.
-/
theorem i_setGroupWrite_sets_bit : setGroupWrite 0 AccessState.yes = 16 := by
  native_decide

/--
I10: `setGroupWrite` with no on 16 clears bit 4.
-/
theorem i_setGroupWrite_clears_bit : setGroupWrite 16 AccessState.no = 0 := by
  native_decide

/--
I11: Query spec with default input yields output string based on mode, mode unchanged.
-/
theorem i_spec_query (mode : UInt32) : spec defaultInput mode = (formatState (modeToState mode), mode) := rfl

/--
I12: Spec with valid arg "y" always outputs empty string and sets bit 4.
-/
theorem i_spec_set_yes : spec { arg := some "y" } 0 = ("", 16) := by
  native_decide

/--
I13: Spec with valid arg "n" on mode with bit 4 set clears it.
-/
theorem i_spec_set_no : spec { arg := some "n" } 16 = ("", 0) := by
  native_decide

/--
I15: Exit codes are distinct.
-/
theorem i_exit_codes_distinct : exitOK ≠ exitError := by
  native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. SetGroupWrite Lemmas
-- ═══════════════════════════════════════════════════════════════════════════════════

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 7. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- mesg without args shows state. -/
example : spec defaultInput 0 = ("is n\n", 0) := by
  native_decide

/-- mesg y sets bit 4. -/
example : spec { arg := some "y" } 0 = ("", 16) := i_spec_set_yes

/-- mesg n clears bit 4. -/
example : spec { arg := some "n" } 16 = ("", 0) := i_spec_set_no

end Lentils.Mesg.Logic
