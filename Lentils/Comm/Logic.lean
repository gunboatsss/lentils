/-
Comm.Logic — Pure comparison logic for `comm`. 0BSD -/
namespace Lentils.Comm.Logic

/-- Suppress which columns? true means suppress. -/
structure SuppressFlags where
  col1 : Bool := false  -- suppress lines unique to file1
  col2 : Bool := false  -- suppress lines unique to file2
  col3 : Bool := false  -- suppress lines common to both
deriving Inhabited

/-- Compute the number of leading tabs for a given column, given which columns are suppressed. -/
def tabsBefore (col : Nat) (flags : SuppressFlags) : Nat :=
  let col1suppressed : Nat := if flags.col1 then 1 else 0
  let col2suppressed : Nat := if flags.col2 then 1 else 0
  match col with
  | 1 => 0  -- column 1 has no tabs before it
  | 2 => if flags.col1 then 0 else 1  -- one tab before col2 unless col1 suppressed
  | 3 => (if flags.col1 then 0 else 1) + (if flags.col2 then 0 else 1)  -- tabs before col3
  | _ => 0

/-- Build a string of n tab characters. -/
def tabs (n : Nat) : String :=
  String.ofList (List.replicate n '\t')

/--
Compare two sorted line lists and produce comm-style output.
Lines unique to file1 are column 1, unique to file2 are column 2,
common lines are column 3. Each column gets tab-prefixed according to
which lower-numbered columns are not suppressed.
-/
def comm (lines1 lines2 : List String) (flags : SuppressFlags := {}) : String :=
  let rec go (l1 : List String) (l2 : List String) (acc : List String) : List String :=
    match l1, l2 with
    | [], [] => acc.reverse
    | [], b :: bs =>
      if flags.col2 then go [] bs acc
      else go [] bs ((tabs (tabsBefore 2 flags) ++ b) :: acc)
    | a :: as, [] =>
      if flags.col1 then go as [] acc
      else go as [] (a :: acc)
    | a :: as, b :: bs =>
      if decide (a < b) then
        if flags.col1 then go as (b :: bs) acc
        else go as (b :: bs) (a :: acc)
      else if decide (b < a) then
        if flags.col2 then go (a :: as) bs acc
        else go (a :: as) bs ((tabs (tabsBefore 2 flags) ++ b) :: acc)
      else
        if flags.col3 then go as bs acc
        else go as bs ((tabs (tabsBefore 3 flags) ++ a) :: acc)
  String.intercalate "\n" (go lines1 lines2 [])

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- comm of two empty lists yields empty output. -/
example : comm [] [] {} = "" := by
  native_decide

/-- comm with col1 suppressed hides lines unique to file1. -/
example : comm ["a"] [] { col1 := true, col2 := false, col3 := false } = "" := by
  native_decide

end Lentils.Comm.Logic


