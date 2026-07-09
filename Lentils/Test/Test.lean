/-
Test — IO wrapper for the `test` utility. 0BSD
-/

import Lentils.Test.Logic

namespace Lentils.Test

open Logic

def run (args : List String) : IO UInt32 := return runPure args

end Lentils.Test
