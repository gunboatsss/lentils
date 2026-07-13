/-
Tty — IO wrapper for the `tty` utility. 0BSD -/
import Lentils.Tty.Logic

namespace Lentils.Tty

def run (_args : List String) : IO UInt32 := do
  -- Read symlink target of /proc/self/fd/0
  let ttyPath ←
    try
      let content ← IO.FS.readFile "/proc/self/fd/0"
      -- On Linux, reading a symlink via readFile returns the link target
      pure (content.trimAscii.toString)
    catch _ =>
      pure ""
  if ttyPath.startsWith "/dev/" then
    IO.println ttyPath
    return 0
  else
    IO.println "not a tty"
    return 1

end Lentils.Tty
