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
    -- Empty parts count as zero (GNU accepts ".5" and "5.").
    -- Integer parts convert with full precision (rounding, never mod 2^64 wrap).
    match body.splitOn "." with
    | [intPart] =>
      let n? := if intPart.isEmpty then some 0 else String.toNat? intPart
      match n? with
      | some (n : Nat) => some (if neg then -(n.toFloat) else n.toFloat)
      | none => none
    | [intPart, fracPart] =>
      let int? := if intPart.isEmpty then some 0 else String.toNat? intPart
      let frac? := if fracPart.isEmpty then some 0 else String.toNat? fracPart
      match int?, frac? with
      | some (int : Nat), some (frac : Nat) =>
        let fracLen := fracPart.length
        let intF := int.toFloat
        let fracF := frac.toFloat / (10.0 ^ (fracLen.toFloat))
        some (if neg then -(intF + fracF) else intF + fracF)
      | _, _ => none
    | _ => none

end Lentils.Common.Float
