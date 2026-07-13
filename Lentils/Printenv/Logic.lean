/-
Printenv.Logic — Pure environment formatting for `printenv`. 0BSD -/
namespace Lentils.Printenv.Logic

/--
Check if a "KEY=VALUE" string matches a requested variable name.
-/
def matchesVar (entry : String) (name : String) : Bool :=
  entry.startsWith (name ++ "=")

/--
Extract the value from a "KEY=VALUE" string.
-/
def extractValue (entry : String) : String :=
  match entry.splitOn "=" with
  | _ :: rest => String.join rest
  | _ => ""

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- matchesVar matches a variable name in a KEY=VALUE pair. -/
example : matchesVar "HOME=/home/user" "HOME" = true := by
  native_decide

example : matchesVar "HOME=/home/user" "PATH" = false := by
  native_decide

end Lentils.Printenv.Logic


