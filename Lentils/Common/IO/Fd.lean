/-
Fd.lean — Raw file-descriptor FFI declarations.
0BSD

Extern declarations for C wrappers in c/coreutils.c.
These are unverified (IO/FFI); differential tests cover correctness.
-/

namespace Lentils.Common.IO.Fd

/-- POSIX open flags (from fcntl.h) -/
def O_RDONLY   : UInt32 := 0
def O_WRONLY   : UInt32 := 1
def O_RDWR     : UInt32 := 2
def O_CREAT    : UInt32 := 0o100
def O_TRUNC    : UInt32 := 0o1000
def O_APPEND   : UInt32 := 0o2000

/-- Default mode for O_CREAT: 0644 (rw-r--r--) -/
def DEFAULT_MODE : UInt32 := 0o644

@[extern "lean_coreutils_open"]
opaque openFile (path : @& String) (flags : UInt32) (mode : UInt32) : IO UInt32

@[extern "lean_coreutils_close"]
opaque closeFd (fd : UInt32) : IO Unit

@[extern "lean_coreutils_read"]
opaque readBytes (fd : UInt32) (n : USize) : IO ByteArray

@[extern "lean_coreutils_write"]
opaque writeBytes (fd : UInt32) (buf : @& ByteArray) : IO UInt32

@[extern "lean_coreutils_ignore_sigpipe"]
opaque ignoreSigpipe : IO Unit

end Lentils.Common.IO.Fd
