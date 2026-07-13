/-
Mesg.Logic — Pure terminal-access logic for `mesg`.
0BSD

This file contains ONLY pure functions — no IO, no FFI.
Formal proofs are at the bottom.
No `sorry` or `admit` allowed.

The `mesg` utility controls whether other users are allowed to send
messages to the terminal device.

POSIX.1-2017, Section "mesg — permit or deny messages":
  - With no operand, writes "is y\n" or "is n\n" to stdout.
  - With operand "y", grants write access to the terminal.
  - With operand "n", denies write access to the terminal.
  - Exit code 0 on success, 1 on error (e.g., not a terminal).
-/

namespace Lentils.Mesg.Logic

/--
Access states for terminal write permission.
-/
inductive AccessState : Type where
  | yes  -- write access allowed (S_IWGRP set)
  | no   -- write access denied (S_IWGRP cleared)
deriving DecidableEq, BEq

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
Exit code for success.
-/
def exitOK : UInt32 := 0

/--
Exit code for error (not a terminal, invalid arg, permission denied).
POSIX convention: 2 for usage/terminal errors.
-/
def exitError : UInt32 := 2

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/--
`parseArg "y"` returns `some AccessState.yes`.
-/
theorem parseArg_y : parseArg "y" = some AccessState.yes := rfl

/--
`parseArg "n"` returns `some AccessState.no`.
-/
theorem parseArg_n : parseArg "n" = some AccessState.no := rfl

/--
`parseArg` returns `none` for invalid arguments.
-/
theorem parseArg_invalid : parseArg "x" = none := rfl

/--
`formatState yes` produces "is y\n".
-/
theorem formatState_yes : formatState AccessState.yes = "is y\n" := rfl

/--
`formatState no` produces "is n\n".
-/
theorem formatState_no : formatState AccessState.no = "is n\n" := rfl

/--
`modeToState` of a mode with S_IWGRP (16) set returns yes.
-/
theorem modeToState_yes : modeToState 16 = AccessState.yes := by
  native_decide

/--
`modeToState` of a mode without S_IWGRP returns no.
-/
theorem modeToState_no : modeToState 0 = AccessState.no := by
  native_decide

/--
`setGroupWrite` with yes sets bit 4.
-/
theorem setGroupWrite_sets_bit : setGroupWrite 0 AccessState.yes = 16 := by
  native_decide

/--
`setGroupWrite` with no clears bit 4.
-/
theorem setGroupWrite_clears_bit : setGroupWrite 16 AccessState.no = 0 := by
  native_decide

/--
`setGroupWrite` preserves other bits.
-/
theorem setGroupWrite_preserves_other : setGroupWrite 0x1F0 AccessState.yes = 0x1F0 ||| 16 := by
  native_decide

end Lentils.Mesg.Logic
