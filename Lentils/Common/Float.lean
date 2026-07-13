/-
Common.Float — Minimal string-to-Float parser for lean-coreutils.
0BSD

Lean 4.31 core does not include a built-in `String.toFloat?`.
This module provides a simple parser for decimal strings.
-/

namespace Lentils.Common.Float

/--
Parse a decimal string into a Float.
Supports optional leading '-' and a single '.'.
Example: "3.14" → 3.14, "42" → 42.0, "-0.5" → -0.5
Returns `none` for invalid strings.
-/
def parse (s : String) : Option Float :=
  let trimmed := (s.trimAscii.toString)
  if trimmed.isEmpty then none
  else
    let neg := if trimmed.startsWith "-" then true else false
    let body : String :=
      if trimmed.startsWith "-" then (trimmed.drop 1).toString
      else if trimmed.startsWith "+" then (trimmed.drop 1).toString
      else trimmed
    match body.splitOn "." with
    | [intPart] =>
      match String.toNat? intPart with
      | some (n : Nat) => some (if neg then -(Nat.toFloat n) else Nat.toFloat n)
      | none => none
    | [intPart, fracPart] =>
      match String.toNat? intPart, String.toNat? fracPart with
      | some (int : Nat), some (frac : Nat) =>
        let fracLen := fracPart.length
        let fracF := Nat.toFloat frac / (10.0 ^ (Nat.toFloat fracLen))
        some (if neg then -(Nat.toFloat int + fracF) else Nat.toFloat int + fracF)
      | _, _ => none
    | _ => none

end Lentils.Common.Float
