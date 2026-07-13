/-
Seq.Logic — Pure sequence generation for `seq`. 0BSD -/
import Lentils.Common.Float

namespace Lentils.Seq.Logic

/- Parse a float string; returns 0.0 on failure. -/
def parseFloat (s : String) : Float :=
  match Lentils.Common.Float.parse s with
  | some f => f
  | none => 0.0

/- Check if a string represents a float with a decimal point. -/
def hasDecimal (s : String) : Bool :=
  s.contains '.'

/-- Generate a sequence from `first` to `last` stepping by `inc`.
Uses an explicit step counter (Nat) to ensure termination. -/
def seq (first last inc : Float) : List Float :=
  if inc == 0.0 then []
  else
    -- Estimate max steps (the number of values to generate)
    let diff := if inc > 0.0 then last - first else first - last
    let maxSteps : Nat :=
      if diff < 0.0 then 1
      else
        let s := (diff / (if inc < 0.0 then -inc else inc)).toUInt64
        let s := if s > 1000000 then 1000000 else s.toNat
        s + 1  -- include both endpoints
    -- Iterate using a descending Nat counter instead of Float recursion
    let rec go (i : Nat) (cur : Float) : List Float :=
      if i = 0 then []
      else
        if inc > 0.0 && cur > last then []
        else if inc < 0.0 && cur < last then []
        else cur :: go (i - 1) (cur + inc)
    go maxSteps first

/-- Format a float for display: strip trailing zeros from decimal representation. -/
def formatFloat (f : Float) : String :=
  let s := toString f
  match s.splitOn "." with
  | [intPart, fracPart] =>
    -- Strip trailing zeros from fracPart
    let rec stripZeros (cs : List Char) : List Char :=
      match cs with
      | [] => []
      | '0' :: rest => stripZeros rest
      | c :: rest => c :: stripZeros rest
    let stripped := stripZeros (fracPart.toList.reverse)
    if stripped.isEmpty then intPart
    else intPart ++ "." ++ (String.ofList stripped.reverse)
  | _ => s

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- hasDecimal detects decimal point in a string. -/
example : hasDecimal "1.5" = true := by
  native_decide

/-- hasDecimal returns false for integer strings. -/
example : hasDecimal "42" = false := by
  native_decide
