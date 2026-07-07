/-
Printf — IO wrapper for the `printf` utility.
0BSD
-/

import Lentils.Printf.Logic

namespace Lentils.Printf

open Logic

def run (args : List String) : IO UInt32 := do
  match args with
  | [] => return 0
  | fmt :: fmtArgs =>
    IO.print (format fmt fmtArgs)
    return 0

end Lentils.Printf
