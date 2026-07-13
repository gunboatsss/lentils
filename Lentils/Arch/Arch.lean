/-
Arch — IO wrapper for the `arch` utility.
0BSD

Prints the machine architecture name by reading /proc filesystem.
IO side effects are confined to this module.
-/

import Lentils.Arch.Logic

namespace Lentils.Arch

/--
Run the `arch` utility.
Reads /proc/sys/kernel/arch (or /proc/sys/kernel/machine as fallback)
and prints the machine architecture.
-/
def run (_args : List String) : IO UInt32 := do
  let arch ←
    try
      let content ← IO.FS.readFile "/proc/sys/kernel/arch"
      pure (content.trimAscii.toString)
    catch _ =>
      try
        let content ← IO.FS.readFile "/proc/sys/kernel/machine"
        pure (content.trimAscii.toString)
      catch _ =>
        pure ""
  if arch == "" then
    IO.eprintln "arch: cannot determine architecture"
    return 1
  else
    IO.println arch
    return 0

end Lentils.Arch
