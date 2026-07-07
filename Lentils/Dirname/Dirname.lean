/-
Dirname — IO wrapper for the `dirname` utility.
0BSD
-/
import Lentils.Dirname.Logic

namespace Lentils.Dirname
open Logic
def run (args : List String) : IO UInt32 := do
  match args with
  | [] => IO.println "."; return 0
  | path :: _ => IO.println (dirname path); return 0
end Lentils.Dirname
