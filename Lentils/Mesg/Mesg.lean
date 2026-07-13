/-
Mesg — IO wrapper for the `mesg` utility.
0BSD

Controls terminal write access for other users.
Uses Mesg.Logic (pure, verified) for access state handling.
IO/FFI side effects are confined to this module.
-/

import Lentils.Common.IO.Native
import Lentils.Mesg.Logic

namespace Lentils.Mesg

open Logic
open Lentils.Common.IO.Native

/--
Check if a file descriptor is a terminal.
Uses the isatty(3) FFI.
-/
def isTerminal (fd : UInt32) : IO Bool := do
  let r ← isatty fd
  pure (r ≠ 0)

/--
Get the terminal device path for a file descriptor.
Uses the ttyname(3) FFI.
Returns empty string if not a terminal.
-/
def getTerminalPath (fd : UInt32) : IO String := do
  let path ← ttyname fd
  pure path

/--
Check the current write access state of a terminal device.
Stat the device and check the S_IWGRP bit.
-/
def checkAccess (path : String) : IO AccessState := do
  let mode ← statMode path
  pure (modeToState mode)

/--
Set the write access state of a terminal device.
-/
def setAccess (path : String) (state : AccessState) : IO Bool := do
  let currentMode ← statMode path
  let newMode := setGroupWrite currentMode state
  try
    chmod path newMode
    pure true
  catch _ =>
    pure false

/--
Run the `mesg` utility with the given arguments.

With no arguments, prints access state.
With "y", grants write access.
With "n", denies write access.
-/
def run (args : List String) : IO UInt32 := do
  -- Check if stdin is a terminal first (matches host behavior: tty check before arg validation)
  let isTty ← isTerminal 0
  if not isTty then
    -- Host mesg exits with 2 silently when not a terminal
    return exitError

  -- Get the terminal device path
  let ttyPath ← getTerminalPath 0
  if ttyPath.isEmpty then
    return exitError

  match args with
  | [] => do
    -- Query mode
    let state ← checkAccess ttyPath
    IO.print (formatState state)
    return exitOK

  | [arg] =>
    match parseArg arg with
    | some state => do
      let ok ← setAccess ttyPath state
      if ok then
        return exitOK
      else
        return exitError
    | none => do
      IO.eprintln s!"mesg: invalid argument '{arg}'"
      IO.eprintln "Usage: mesg [y|n]"
      return exitError

  | _ => do
    IO.eprintln "mesg: too many arguments"
    IO.eprintln "Usage: mesg [y|n]"
    return exitError

end Lentils.Mesg
