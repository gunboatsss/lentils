/-
Comm.Logic — Pure comparison logic for `comm`. 0BSD -/
namespace Lentils.Comm.Logic

/-- Suppress which columns? true means suppress. -/
structure SuppressFlags where
  col1 : Bool := false  -- suppress lines unique to file1
  col2 : Bool := false  -- suppress lines unique to file2
  col3 : Bool := false  -- suppress lines common to both
deriving Inhabited

/--
Compare two sorted line lists and produce comm-style output.
Lines unique to file1 get tab prefix, lines unique to file2 get two tabs,
lines common to both get no prefix.
-/
def comm (lines1 lines2 : List String) (flags : SuppressFlags := {}) : String :=
  let rec go (l1 : List String) (l2 : List String) (acc : List String) : List String :=
    match l1, l2 with
    | [], [] => acc.reverse
    | [], b :: bs =>
      if flags.col2 then go [] bs acc
      else go [] bs (s!"\t\t{b}" :: acc)
    | a :: as, [] =>
      if flags.col1 then go as [] acc
      else go as [] (a :: acc)
    | a :: as, b :: bs =>
      if decide (a < b) then
        if flags.col1 then go as (b :: bs) acc
        else go as (b :: bs) (a :: acc)
      else if decide (b < a) then
        if flags.col2 then go (a :: as) bs acc
        else go (a :: as) bs (s!"\t\t{b}" :: acc)
      else
        if flags.col3 then go as bs acc
        else go as bs (a :: acc)
  String.intercalate "\n" (go lines1 lines2 [])

end Lentils.Comm.Logic
