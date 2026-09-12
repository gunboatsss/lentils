/-
Printf.Logic — Verified pure format-string processing for `printf`.
0BSD

Structure:
  1. State types      — PrintfInput
  2. Specification    — spec (format string substitution)
  3. Lemmas          — structural properties of go
  4. Invariants       — parametric theorems
  5. Concrete corollaries

POSIX.1-2017 §printf: writes a format string with substituted arguments.
% is the escape character. %% produces a literal %.
%s, %d, %i substitute the next argument.
Unknown format specifiers are silently ignored.

No IO, no FFI, no `sorry` or `admit`.
-/

import Lentils.Common.Spec

namespace Lentils.Printf.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for printf.
-/
structure PrintfInput where
  fmt : String
  args : List String
  deriving Inhabited, BEq, Repr

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Core Format Processing
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
The core format-processing recursion.
Processes a list of characters, consuming arguments as needed.
-/
def go (cs : List Char) (as : List String) : String :=
  match cs with
  | [] => ""
  | '%' :: '%' :: rest => "%" ++ go rest as
  | '%' :: 's' :: rest =>
    match as with
    | [] => go rest []
    | arg :: restArgs => arg ++ go rest restArgs
  | '%' :: 'd' :: rest =>
    match as with
    | [] => go rest []
    | arg :: restArgs => arg ++ go rest restArgs
  | '%' :: 'i' :: rest =>
    match as with
    | [] => go rest []
    | arg :: restArgs => arg ++ go rest restArgs
  | '%' :: _ :: rest => go rest as
  | c :: rest => String.ofList [c] ++ go rest as

/--
Format a string with positional arguments.
-/
def format (fmt : String) (args : List String) : String :=
  go fmt.toList args

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification (cont.)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Specification: format the format string with the given arguments.
-/
def spec (input : PrintfInput) : String :=
  format input.fmt input.args

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Lemmas — direct consequences of `go`'s definition
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
go on "%%" produces "%" and recurses.
-/
theorem go_escape (rest : List Char) (as : List String) :
    go ('%' :: '%' :: rest) as = "%" ++ go rest as := rfl

/--
go on "%s" with a non-empty arg list substitutes the first arg.
-/
theorem go_subst_s (rest : List Char) (arg : String) (restArgs : List String) :
    go ('%' :: 's' :: rest) (arg :: restArgs) = arg ++ go rest restArgs := rfl

/--
go on "%d" with a non-empty arg list substitutes the first arg.
-/
theorem go_subst_d (rest : List Char) (arg : String) (restArgs : List String) :
    go ('%' :: 'd' :: rest) (arg :: restArgs) = arg ++ go rest restArgs := rfl

/--
go on "%i" with a non-empty arg list substitutes the first arg.
-/
theorem go_subst_i (rest : List Char) (arg : String) (restArgs : List String) :
    go ('%' :: 'i' :: rest) (arg :: restArgs) = arg ++ go rest restArgs := rfl

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Empty format → empty string.
-/
theorem i_empty : format "" [] = "" := rfl

/--
I2: Empty format → empty string regardless of args.
-/
theorem i_empty_args (args : List String) : format "" args = "" := rfl

/--
I3: %% → literal % followed by the processed rest. Parametric over rest.
-/
theorem i_escape (rest : String) : format ("%%" ++ rest) [] = "%" ++ format rest [] := by
  calc
    format ("%%" ++ rest) [] = go (("%%" ++ rest).toList) [] := rfl
    _ = go ('%' :: '%' :: rest.toList) [] := by simp
    _ = "%" ++ go rest.toList [] := go_escape (rest.toList) []
    _ = "%" ++ format rest [] := rfl

/--
I4: %s with one arg → arg followed by the processed rest. Parametric.
-/
theorem i_subst_s (arg : String) (rest : String) :
    format ("%s" ++ rest) [arg] = arg ++ format rest [] := by
  calc
    format ("%s" ++ rest) [arg] = go (("%s" ++ rest).toList) [arg] := rfl
    _ = go ('%' :: 's' :: rest.toList) [arg] := by simp
    _ = arg ++ go rest.toList [] := go_subst_s (rest.toList) arg []
    _ = arg ++ format rest [] := rfl

/--
I5: %d with one arg → arg followed by the processed rest. Parametric.
-/
theorem i_subst_d (arg : String) (rest : String) :
    format ("%d" ++ rest) [arg] = arg ++ format rest [] := by
  calc
    format ("%d" ++ rest) [arg] = go (("%d" ++ rest).toList) [arg] := rfl
    _ = go ('%' :: 'd' :: rest.toList) [arg] := by simp
    _ = arg ++ go rest.toList [] := go_subst_d (rest.toList) arg []
    _ = arg ++ format rest [] := rfl

/--
I6: %i with one arg → arg followed by the processed rest. Parametric.
-/
theorem i_subst_i (arg : String) (rest : String) :
    format ("%i" ++ rest) [arg] = arg ++ format rest [] := by
  calc
    format ("%i" ++ rest) [arg] = go (("%i" ++ rest).toList) [arg] := rfl
    _ = go ('%' :: 'i' :: rest.toList) [arg] := by simp
    _ = arg ++ go rest.toList [] := go_subst_i (rest.toList) arg []
    _ = arg ++ format rest [] := rfl

/--
I7: Multiple %s consume multiple args in order. Parametric.
-/
theorem i_multi_subst (a b : String) : format "%s%s" [a, b] = a ++ b := by
  calc
    format "%s%s" [a, b] = go ("%s%s".toList) [a, b] := rfl
    _ = go ('%' :: 's' :: '%' :: 's' :: []) [a, b] := rfl
    _ = a ++ go ('%' :: 's' :: []) [b] := by rw [go_subst_s]
    _ = a ++ (b ++ go [] []) := by rw [go_subst_s]
    _ = a ++ b := by
      have h : go [] [] = "" := rfl
      simp [h, String.append_assoc]

/--
I8: Extra args are silently ignored.
-/
theorem i_extra_args (arg : String) : format "" [arg] = "" := rfl

/--
I11: Unknown format specifier (%x) is silently dropped.
-/
theorem i_unknown_spec_ground : format "%x" [] = "" := by native_decide

/--
I12: Plain text with no format specifiers passes through unchanged (list version).
-/
theorem i_plain_text_list (cs : List Char) (h : '%' ∉ cs) : go cs [] = String.ofList cs := by
  induction cs with
  | nil => rfl
  | cons c cs ih =>
    unfold go
    have hc : c ≠ '%' := by
      intro hc; apply h; simp [hc]
    simp [hc, ih (by
      intro hmem; apply h; simp [hmem])]

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- printf "" = "" -/
example : format "" [] = "" := i_empty

/-- printf "hello" = "hello" (no format specifiers) -/
example : format "hello" [] = "hello" := by
  native_decide

/-- printf "%%" = "%" -/
example : format "%%" [] = "%" := by
  calc
    format "%%" [] = format ("%%" ++ "") [] := rfl
    _ = "%" ++ format "" [] := i_escape ""
    _ = "%" := rfl

/-- printf "%s" ["world"] = "world" -/
example : format "%s" ["world"] = "world" := by
  calc
    format "%s" ["world"] = format ("%s" ++ "") ["world"] := rfl
    _ = "world" ++ format "" [] := i_subst_s "world" ""
    _ = "world" := rfl

/-- printf "%d" ["42"] = "42" -/
example : format "%d" ["42"] = "42" := by
  calc
    format "%d" ["42"] = format ("%d" ++ "") ["42"] := rfl
    _ = "42" ++ format "" [] := i_subst_d "42" ""
    _ = "42" := rfl

/-- printf "hello %s" ["world"] = "hello world" -/
example : format "hello %s" ["world"] = "hello world" := by
  native_decide

end Lentils.Printf.Logic
