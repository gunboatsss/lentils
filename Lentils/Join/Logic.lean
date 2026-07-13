/-
Join.Logic — Pure join logic for `join`. 0BSD -/
namespace Lentils.Join.Logic

/--
Extract the join field from a line.
Fields are separated by delimiter; field index is 1-based (default 1).
-/
def extractField (line : String) (delim : String) (field : Nat) : String :=
  let parts := line.splitOn delim
  match parts.drop (field - 1) with
  | x :: _ => x
  | [] => ""

/--
Join two sorted line lists on a common field.
For each pair of lines with matching join fields, output the joined line:
  join field + rest of file1 line + rest of file2 line
-/
def join (lines1 lines2 : List String) (delim : String := " ")
         (field1 field2 : Nat := 1) : String :=
  let rec go (l1 : List String) (l2 : List String) (acc : List String) : List String :=
    match l1, l2 with
    | [], _ => acc.reverse
    | _, [] => acc.reverse
    | a :: as, b :: bs =>
      let keyA := extractField a delim field1
      let keyB := extractField b delim field2
      if decide (keyA < keyB) then
        go as (b :: bs) acc
      else if decide (keyB < keyA) then
        go (a :: as) bs acc
      else
        -- Keys match: output the joined line
        let restA := (a.splitOn delim).drop field1
        let restB := (b.splitOn delim).drop field2
        let joined := String.intercalate delim ([keyA] ++ restA ++ restB)
        go as bs (joined :: acc)
  String.intercalate "\n" (go lines1 lines2 [])

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- Empty files produce empty join. -/
example : join [] [] " " = "" := by
  native_decide

end Lentils.Join.Logic


