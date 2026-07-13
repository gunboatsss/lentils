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

end Lentils.Printenv.Logic
