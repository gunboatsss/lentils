/-
Readlink.Logic - Pure logic for readlink. 0BSD -/
namespace Lentils.Readlink.Logic

/--
Validate that a path argument is provided.
-/
def getPath (args : List String) : Option String :=
  args.head?

end Lentils.Readlink.Logic
