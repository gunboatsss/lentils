/-
Basename — IO wrapper for the `basename` utility.
0BSD
-/
import Lentils.Basename.Logic

namespace Lentils.Basename
open Logic
def run (args : List String) : IO UInt32 := do
  match args with
  | [] => IO.println ""; return 0
  | path :: [] => IO.println (basename path); return 0
  | path :: suffix :: _ => IO.println (basename path suffix); return 0
end Lentils.Basename
