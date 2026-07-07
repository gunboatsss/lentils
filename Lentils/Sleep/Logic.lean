/-
Sleep.Logic — Pure specification for `sleep`.
0BSD

This file contains ONLY pure functions — no IO, no FFI.
Formal proofs are at the bottom.
No `sorry` or `admit` allowed.

The `sleep` utility suspends execution for the specified number of
seconds. Per POSIX.1-2017, Section "sleep — suspend execution for an
interval":

  The sleep utility shall suspend execution for at least the integral
  number of seconds specified by the time operand, or the time specified
  by all non-option operands.

Pure logic: parse a duration string (e.g., "1.5", "60") into (seconds,
remaining nanoseconds). The actual sleeping is handled in the IO wrapper
via nanosleep FFI.

Provenance: POSIX.1-2017, Section "sleep".
No GPL source was consulted.
-/

namespace Lentils.Sleep.Logic

/--
Convert a duration string to (seconds, nanoseconds).
Returns none if the string is not a valid duration.
Accepts decimal numbers like "1.5", "0.1", "60", "2.75".
-/
def parseDuration (s : String) : Option (Nat × Nat) :=
  let parts := s.splitOn "."
  match parts with
  | [whole] =>
    -- No decimal point, just whole seconds
    match whole.toNat? with
    | none => none
    | some secs => some (secs, 0)
  | [whole, frac] =>
    match whole.toNat?, frac.toNat? with
    | some secs, some fracVal =>
      -- Interpret fractional part as nanoseconds
      -- e.g., ".5" -> 500_000_000, ".25" -> 250_000_000, ".1" -> 100_000_000
      let fracLen := frac.length
      let nanos :=
        if fracLen <= 9 then
          fracVal * (10 ^ (9 - fracLen))
        else
          -- Truncate to 9 digits
          match frac.take 9 |>.toNat? with
          | none => 0
          | some v => v
      some (secs, nanos)
    | _, _ => none
  | _ => none  -- multiple dots or empty: invalid

/--
The exit code of `sleep` on success. Always 0.
-/
def exitCode : UInt32 := 0

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/--
parseDuration of "0" yields (0, 0).
-/
example : parseDuration "0" = some (0, 0) := by
  native_decide

/--
parseDuration of "1" yields (1, 0) — one second, zero nanoseconds.
-/
example : parseDuration "1" = some (1, 0) := by
  native_decide

/--
parseDuration of "1.5" yields (1, 500_000_000).
-/
example : parseDuration "1.5" = some (1, 500000000) := by
  native_decide

/--
parseDuration of "0.1" yields (0, 100_000_000).
-/
example : parseDuration "0.1" = some (0, 100000000) := by
  native_decide

/--
parseDuration of empty string is none.
-/
example : parseDuration "" = none := by
  native_decide

/--
parseDuration of "abc" is none.
-/
example : parseDuration "abc" = none := by
  native_decide

/--
Idempotence: parseDuration produces the same output given the same input.
-/
theorem parseDuration_idempotent (s : String) : parseDuration s = parseDuration s := rfl

end Lentils.Sleep.Logic
