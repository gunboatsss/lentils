/-
More — IO wrapper for the `more` utility.
0BSD

Paginates stdin to stdout, pausing after each screenful.
Uses More.Logic (pure, verified) for paging calculations.
IO/FFI side effects are confined to this module.
-/

import Lentils.More.Logic

namespace Lentils.More

open Logic

/--
Read the terminal height from the LINES environment variable,
falling back to the default of 24.
-/
def getTermHeight : IO Nat := do
  let env ← IO.getEnv "LINES"
  match env with
  | some s =>
    match s.toNat? with
    | some n => if n > 0 then pure n else pure defaultLines
    | none => pure defaultLines
  | none => pure defaultLines

/--
Open /dev/tty for reading commands. If /dev/tty cannot be opened
(e.g., not in a terminal), returns `none`, which causes the pager
to output all remaining lines without pausing.
-/
def openTty : IO (Option IO.FS.Stream) := do
  try
    let h ← IO.FS.Handle.mk (System.FilePath.mk "/dev/tty") IO.FS.Mode.read
    pure (some (IO.FS.Stream.ofHandle h))
  catch _ =>
    pure none

/--
Read a single byte from a TTY stream.
Returns 255 (0xFF) on error.
-/
def readTtyByte (tty : IO.FS.Stream) : IO UInt8 := do
  try
    let buf ← tty.read 1
    if buf.size > 0 then
      pure buf[0]!
    else
      pure 0xFF
  catch _ =>
    pure 0xFF

/--
Wait for a command keypress from the user.
Returns `true` if the user wants to quit.
-/
def waitForKey (tty : IO.FS.Stream) : IO Bool := do
  let b ← readTtyByte tty
  -- Also consume rest of line if the user typed more than one char
  try
    _ ← tty.read 256
  catch _ =>
    pure ()
  pure (isQuit b)

/--
Output a page of lines, then prompt for continuation.
Returns `true` if the user wants to quit.
Lines are drawn from `lines[start:]`.
-/
partial def outputPage (lines : Array String) (start : Nat) (height : Nat) : IO (Bool × Nat) := do
  let remaining := lines.size - start
  let pSize := pageSize height
  let pageLen := if remaining < pSize then remaining else pSize
  -- Print the page
  for i in [0:pageLen] do
    IO.println lines[start + i]!
  let newStart := start + pageLen
  if newStart ≥ lines.size then
    pure (false, newStart)  -- done, no more pages
  else
    -- Show page indicator
    let indicator := pageIndicator newStart lines.size
    if indicator ≠ "" then
      IO.eprintln indicator
    -- Wait for key
    let ttyOpt ← openTty
    match ttyOpt with
    | none =>
      -- No tty, just keep going
      outputPage lines newStart height
    | some tty => do
      let quit ← waitForKey tty
      if quit then
        pure (true, newStart)
      else
        outputPage lines newStart height

/--
Run the `more` utility with the given arguments.
If no arguments, reads from stdin. Otherwise reads from files.
-/
def run (_args : List String) : IO UInt32 := do
  let height ← getTermHeight

  -- Read input from stdin
  let input ←
    try
      IO.FS.readFile "/dev/stdin"
    catch _ =>
      pure ""
  
  let lines := input.splitOn "\n"
  let linesArr := lines.toArray

  if linesArr.isEmpty then
    return 0

  -- Output pages
  let (quit, _) ← outputPage linesArr 0 height
  if quit then
    return 0
  else
    return 0

end Lentils.More
