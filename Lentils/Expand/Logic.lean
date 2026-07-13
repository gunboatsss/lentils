/-
Expand.Logic — Pure tab-expansion logic for `expand`. 0BSD -/
namespace Lentils.Expand.Logic

/--
Convert tabs to spaces in a single line.
Default tab stop is 8.
-/
def expandLine (line : String) (tabSize : Nat := 8) : String :=
  let rec go (chars : List Char) (col : Nat) (acc : List Char) : List Char :=
    match chars with
    | [] => acc.reverse
    | '\t' :: rest =>
      let spacesNeeded := tabSize - (col % tabSize)
      let spaces := List.replicate spacesNeeded ' '
      go rest (col + spacesNeeded) (spaces ++ acc)
    | c :: rest =>
      go rest (col + 1) (c :: acc)
  String.ofList (go line.toList 0 [])

/--
Expand tabs in multi-line input.
-/
def expand (input : String) (tabSize : Nat := 8) : String :=
  let lines := input.splitOn "\n"
  let expanded := lines.map (λ l => expandLine l tabSize)
  String.intercalate "\n" expanded

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- A line with no tabs is unchanged. -/
example : expandLine "hello" 8 = "hello" := by
  native_decide

/-- Empty line stays empty. -/
example : expandLine "" 8 = "" := by
  native_decide

end Lentils.Expand.Logic


