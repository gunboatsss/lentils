/-
Sys.lean — Raw system-level FFI declarations (getcwd, nanosleep, etc.).
0BSD

Extern declarations for C wrappers in c/coreutils.c.
These are unverified (IO/FFI); differential tests cover correctness.
-/

namespace Lentils.Common.IO.Sys

/--
getcwd(3): Returns the current working directory as a String.
Corresponds to lean_coreutils_getcwd in c/coreutils.c.
-/
@[extern "lean_coreutils_getcwd"]
opaque getcwd : IO String

/--
nanosleep(2): Sleep for the given number of nanoseconds.
Returns 0 on success, otherwise remaining nanoseconds if interrupted.
Corresponds to lean_coreutils_nanosleep in c/coreutils.c.
-/
@[extern "lean_coreutils_nanosleep"]
opaque nanosleep (ns : UInt64) : IO UInt64

end Lentils.Common.IO.Sys
