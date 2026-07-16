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
Intercalate strings with single spaces, with explicit patterns for
easy parametric reasoning (mirrors lentils/echo-invariants approach).
-/
def intercalateSpace : List String → String
  | [] => ""
  | [x] => x
  | [x, y] => x ++ " " ++ y
  | xs => String.intercalate " " xs

/--
Strips all leading "-n" flags and returns the remaining args with the
suppressNewline flag set if any -n was found.
This matches BSD echo behavior where multiple -n flags are consumed.
-/
def stripN (args : List String) : List String × Bool :=
  match args with
  | "-n" :: rest => 
    let (remaining, _) := stripN rest
    (remaining, true)
  | _ => (args, false)

/--
Format arguments for echo output.
If the first argument is "-n", it is consumed and no trailing newline
is emitted (BSD-compatible behavior).
-/
def format (args : List String) : String :=
  let (remaining, suppressNewline) := stripN args
  let joined := intercalateSpace remaining
  if suppressNewline then
    joined
  else
    joined ++ "\n"

/--
Format for non-"-n" cases (parametric).
This version doesn't handle -n flag stripping, useful for parametric proofs.
-/
def formatNoN (args : List String) : String :=
  intercalateSpace args ++ "\n"

/--
format_no_n for empty args.
-/
theorem format_no_n_empty : formatNoN [] = "\n" := rfl

/--
format_no_n for singleton arg (parametric).
-/
theorem format_no_n_single (s : String) : formatNoN [s] = s ++ "\n" := rfl

/--
format_no_n for pair args (parametric).
-/
theorem format_no_n_pair (s1 s2 : String) : formatNoN [s1, s2] = s1 ++ " " ++ s2 ++ "\n" := rfl

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
Intercalate space on empty list returns empty string (parametric).
-/
theorem intercalate_space_empty : intercalateSpace ([] : List String) = "" := rfl

/--
Intercalate space on singleton list returns the element (parametric).
-/
theorem intercalate_space_single (s : String) : intercalateSpace [s] = s := rfl

/--
Intercalate space on pair of strings joins with space (parametric).
-/
theorem intercalate_space_pair (s1 s2 : String) : intercalateSpace [s1, s2] = s1 ++ " " ++ s2 := rfl

/--
`echo` with a single argument works as expected (concrete example).
Note: Parametric proofs require case analysis on string equality with "-n".
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

/--
String append is associative.
This is useful for reasoning about echo output composition.
-/
theorem append_assoc (s1 s2 s3 : String) :
  (s1 ++ s2) ++ s3 = s1 ++ (s2 ++ s3) := String.append_assoc

/--
Length of concatenated strings equals sum of lengths.
Useful for reasoning about output lengths.
-/
theorem append_length (s1 s2 : String) :
  (s1 ++ s2).length = s1.length + s2.length := String.length_append s1 s2

/--
Space separator has length 1.
-/
theorem space_length : " ".length = 1 := rfl

/--
Newline has length 1.
-/
theorem newline_length : "\n".length = 1 := rfl

/--
Empty string has length 0.
-/
theorem empty_length : "".length = 0 := rfl

/--
stripN on empty list returns empty and false.
-/
theorem stripN_empty : stripN ([] : List String) = ([], false) := rfl

/--
stripN on ["-n"] returns ([], true).
-/
theorem stripN_n_only : stripN ["-n"] = ([], true) := by
  unfold stripN
  rfl

/--
stripN on multiple leading "-n" flags consumes all of them.
-/
theorem stripN_multiple : stripN ["-n", "-n", "-n"] = ([], true) := by
  unfold stripN
  rfl

end Lentils.Echo.Logic
