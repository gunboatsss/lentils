/-
Yes.Logic — Pure specification for `yes`.
0BSD

This file contains ONLY pure functions — no IO, no FFI.
Formal proofs are at the bottom.
No `sorry` or `admit` allowed.

The `yes` utility repeatedly outputs a line consisting of the specified
string, or "y" if no arguments are given, until killed. Per POSIX.1-2017,
Section "yes — write a string repeatedly":

  The yes utility shall output a string repeatedly. If operands are
  specified, the string is the concatenation of the operands separated by
  single space characters. Otherwise, the string is "y".

Pure logic: given a list of arguments, produce the string to repeat.
The infinite repetition is handled in the IO wrapper.

Provenance: POSIX.1-2017, Section "yes".
No GPL source was consulted.
-/

namespace Lentils.Yes.Logic

/--
Determine the string to repeat.
If no operands, returns "y". Otherwise concatenates with spaces.
-/
def message (args : List String) : String :=
  match args with
  | [] => "y"
  | _  => String.intercalate " " args

/--
The exit code of `yes` when terminated. Always 0.
yes runs forever until killed (SIGPIPE or SIGINT).
-/
def exitCode : UInt32 := 0

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/--
With no arguments, message is "y".
-/
theorem message_empty : message [] = "y" := rfl

/--
With a single argument, message is that argument.
-/
theorem message_single (s : String) : message [s] = s := rfl

/--
With multiple arguments, message joins them with spaces.
-/
example : message ["hello", "world"] = "hello world" := rfl

/--
The exit code is always zero.
-/
theorem exitCode_is_zero : exitCode = 0 := rfl

/--
Idempotence: message produces the same output given the same input.
-/
theorem message_idempotent (args : List String) : message args = message args := rfl

end Lentils.Yes.Logic
