/-
Dir — IO wrapper for the `dir` utility.
0BSD

`dir` is equivalent to `ls -C` (columnar output).
Simply delegates to `Lentils.Ls.run` with default options.

Provenance: GNU coreutils, `dir` is a separate binary that behaves like `ls -C`.
No GPL source was consulted.
-/

import Lentils.Ls.Ls

namespace Lentils.Dir

/--
Run the `dir` utility.

Delegates to `Lentils.Ls.run` with default listing options (equivalent to `ls -C`).
-/
def run (args : List String) : IO UInt32 :=
  Lentils.Ls.run args

end Lentils.Dir
