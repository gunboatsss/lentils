/-
Unexpand.Logic — Pure space-to-tab conversion logic for `unexpand`. 0BSD -/
namespace Lentils.Unexpand.Logic

/--
Convert leading spaces to tabs in a single line.
Default tab stop is 8.
Processes one character at a time, accumulating space runs
and emitting tabs at tab-stop boundaries.
-/
def unexpandLine (line : String) (tabSize : Nat := 8) : String :=
  let chars := line.toList
  let len := chars.length
  -- i: current position, spaceRun: consecutive spaces seen so far, acc: output (reversed)
  let rec go (i : Nat) (spaceRun : Nat) (acc : List Char) : List Char :=
    if i ≥ len then
      -- Flush remaining spaces, then reverse acc
      List.replicate spaceRun ' ' ++ acc.reverse
    else
      match chars.drop i with
      | ' ' :: _ =>
        let spaceRun' := spaceRun + 1
        if spaceRun' = tabSize then
          -- Emit a tab
          go (i + 1) 0 ('\t' :: acc)
        else
          go (i + 1) spaceRun' acc
      | c :: _ =>
        -- Emit accumulated spaces as spaces, then the character
        let spaces := List.replicate spaceRun ' '
        go (i + 1) 0 (c :: spaces ++ acc)
      | [] => List.replicate spaceRun ' ' ++ acc.reverse
    termination_by len - i
  String.ofList (go 0 0 [])

/--
Convert leading spaces to tabs in multi-line input.
-/
def unexpand (input : String) (tabSize : Nat := 8) : String :=
  let lines := input.splitOn "\n"
  let converted := lines.map (λ l => unexpandLine l tabSize)
  String.intercalate "\n" converted

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- A line with no leading spaces is unchanged. -/
example : unexpandLine "hello" 8 = "hello" := by
  native_decide

/-- Empty line stays empty. -/
example : unexpandLine "" 8 = "" := by
  native_decide

end Lentils.Unexpand.Logic
