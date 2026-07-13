/-
Logname.Logic — Pure login-name logic for `logname`. 0BSD -/
namespace Lentils.Logname.Logic

def isValidLoginName (s : String) : Bool :=
  s ≠ "" && !s.contains '\n'

end Lentils.Logname.Logic
