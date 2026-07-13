/-
Whoami.Logic — Pure identity logic for `whoami`. 0BSD -/
namespace Lentils.Whoami.Logic

def isValidUserName (s : String) : Bool :=
  s ≠ "" && !s.contains '\n'

end Lentils.Whoami.Logic
