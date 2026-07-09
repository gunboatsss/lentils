/-
Test — IO wrapper for the `test` utility. 0BSD
-/

import Lentils.Test.Logic

namespace Lentils.Test

open Logic

-- Strip trailing "]" when invoked via the `[` form (arg[0] is not the program name,
-- the shell passes `[` args directly including the closing `]`).
def run (args : List String) : IO UInt32 :=
  let cleaned := match args.reverse with
    | "]" :: rest => rest.reverse
    | _ => args
  return runPure cleaned

end Lentils.Test
