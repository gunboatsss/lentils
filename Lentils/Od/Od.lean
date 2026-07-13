/-
Od — IO wrapper for the `od` utility. 0BSD -/
import Lentils.Od.Logic

namespace Lentils.Od

open Logic

def run (_args : List String) : IO UInt32 := do
  let data ←
    try IO.FS.readBinFile "/dev/stdin"
    catch _ => pure ByteArray.empty
  let result := octalDump data
  IO.println result
  return 0

end Lentils.Od
