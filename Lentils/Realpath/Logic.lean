/-
Realpath.Logic — Pure logic for `realpath`. 0BSD -/
namespace Lentils.Realpath.Logic

/--
Validate that a path argument is provided.
-/
def getPath (args : List String) : Option String :=
  args.head?

end Lentils.Realpath.Logic
