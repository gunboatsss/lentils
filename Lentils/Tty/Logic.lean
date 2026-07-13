/-
Tty.Logic — Pure tty-checking logic for `tty`. 0BSD -/
namespace Lentils.Tty.Logic

def isTty (path : String) : Bool :=
  path.startsWith "/dev/"

end Lentils.Tty.Logic
