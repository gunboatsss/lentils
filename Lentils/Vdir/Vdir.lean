/-
Vdir — IO wrapper for the `vdir` utility.
0BSD

`vdir` is equivalent to `ls -l` (long format).
Simply delegates to `Lentils.Ls.run` with `-l` prepended.

Provenance: GNU coreutils, `vdir` is a separate binary that behaves like `ls -l`.
No GPL source was consulted.
-/

import Lentils.Ls.Ls

namespace Lentils.Vdir

/--
Run the `vdir` utility.

Delegates to `Lentils.Ls.run` with `-l` prepended (long listing format).
-/
def run (args : List String) : IO UInt32 :=
  Lentils.Ls.run ("-l" :: args)

end Lentils.Vdir
