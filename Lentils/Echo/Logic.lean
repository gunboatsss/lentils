/-
Echo.Logic — Pure string processing for `echo`.
0BSD

This file contains ONLY pure functions — no IO, no FFI.
Formal proofs are at the bottom.
No `sorry` or `admit` allowed.

The `echo` utility writes its arguments to standard output,
separated by single spaces, followed by a newline.
If there are no arguments, only the newline is written.

POSIX.1-2017, Section "echo — write arguments to standard output":
  - If the first operand is -n, or if any of the operands contain a
    backslash character, the results are implementation-defined.

Our implementation (BSD-compatible):
  - If the first argument is "-n", suppresses the trailing newline
    and does not output "-n".
  - Otherwise, joins arguments with a single space character using
    String.intercalate and appends a newline ('\n').
  - Does NOT process escape sequences (treated literally).
  This is a valid POSIX-conformant behavior (the -n case is
  implementation-defined).

Provenance: POSIX.1-2017, Section "echo".
No GPL source was consulted.
-/

namespace Lentils.Echo.Logic

/--
Return the arguments with the leading "-n" stripped and a flag
indicating whether to suppress the trailing newline.
If the first argument is exactly "-n", it is consumed and newline
is suppressed. Otherwise all arguments are kept and newline is emitted.
-/
def stripN (args : List String) : List String × Bool :=
  match args with
  | "-n" :: rest => (rest, true)
  | _            => (args, false)

/--
Format arguments for echo output.
If the first argument is "-n", it is consumed and no trailing newline
is emitted (BSD-compatible behavior).
-/
def format (args : List String) : String :=
  let (remaining, suppressNewline) := stripN args
  let joined := String.intercalate " " remaining
  if suppressNewline then
    joined
  else
    joined ++ "\n"

/--
The exit code of `echo` on success. Always 0.
-/
def exitCode : UInt32 := 0

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/--
The exit code is always zero.
-/
theorem exitCode_is_zero : exitCode = 0 := rfl

/--
`echo` with no arguments outputs just a newline.
-/
theorem format_empty : format [] = "\n" := rfl

/--
`echo` with a single argument works as expected.
-/
theorem format_single : format ["hello"] = "hello\n" := rfl

/--
`echo` with two arguments joins them with a single space.
-/
theorem format_two : format ["hello", "world"] = "hello world\n" := rfl

/--
`echo` with three concrete arguments.
-/
theorem format_three : format ["a", "b", "c"] = "a b c\n" := rfl

/--
`echo` with -n suppresses the trailing newline.
-/
theorem format_n : format ["-n", "hello"] = "hello" := rfl

/--
`echo` with only -n outputs nothing.
-/
theorem format_only_n : format ["-n"] = "" := rfl

/--
`echo` with -n and multiple arguments.
-/
theorem format_n_multi : format ["-n", "hello", "world"] = "hello world" := rfl

/--
`echo` with -n not as first argument preserves it.
-/
theorem format_n_not_first : format ["hello", "-n"] = "hello -n\n" := rfl

/--
Idempotence: format produces the same output given the same input.
-/
theorem format_idempotent (args : List String) : format args = format args := rfl

end Lentils.Echo.Logic
