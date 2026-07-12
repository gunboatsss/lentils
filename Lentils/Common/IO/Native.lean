/-
Native.lean — POSIX I/O using Lean 4 native APIs (IO.FS.Handle, IO.FS.Stream).
0BSD

Replaces the C FFI wrappers in c/coreutils.c with Lean's built-in I/O.
Keeps only ignoreSigpipe as FFI since there's no Lean equivalent for SIGPIGN.
-/

namespace Lentils.Common.IO.Native

/--
A file/stream for chunked I/O.
Wraps either an IO.FS.Stream (stdin/stdout/stderr) or an IO.FS.Handle (opened files).
-/
structure File where
  stream : IO.FS.Stream
  isStdio : Bool  -- true for stdin/stdout/stderr to skip GC-based close

/--
Open a file for reading.
Returns a File handle, or throws on error.
-/
def openFileRead (path : String) : IO File := do
  let h ← IO.FS.Handle.mk (System.FilePath.mk path) IO.FS.Mode.read
  let s := IO.FS.Stream.ofHandle h
  return { stream := s, isStdio := false }

/--
Open a file for writing (truncates).
Returns a File handle, or throws on error.
-/
def openFileWrite (path : String) : IO File := do
  let h ← IO.FS.Handle.mk (System.FilePath.mk path) IO.FS.Mode.write
  let s := IO.FS.Stream.ofHandle h
  return { stream := s, isStdio := false }

/--
Open a file for appending.
Returns a File handle, or throws on error.
-/
def openFileAppend (path : String) : IO File := do
  let h ← IO.FS.Handle.mk (System.FilePath.mk path) IO.FS.Mode.append
  let s := IO.FS.Stream.ofHandle h
  return { stream := s, isStdio := false }

/-- Get stdin as a File. -/
def stdin : IO File := do
  let s ← IO.getStdin
  return { stream := s, isStdio := true }

/-- Get stdout as a File. -/
def stdout : IO File := do
  let s ← IO.getStdout
  return { stream := s, isStdio := true }

/-- Get stderr as a File. -/
def stderr : IO File := do
  let s ← IO.getStderr
  return { stream := s, isStdio := true }

/--
Read up to `n` bytes from a file.
Returns empty ByteArray at EOF.
-/
def readBytes (f : File) (n : USize) : IO ByteArray :=
  f.stream.read n

/--
Read all bytes from a file until EOF.
Uses 64KB chunks.
-/
partial def readAll (f : File) (bufSize : USize := 65536) : IO ByteArray := do
  let chunk ← readBytes f bufSize
  if chunk.isEmpty then
    return ByteArray.empty
  else
    return chunk ++ (← readAll f bufSize)

/--
Write bytes to a file.
-/
def writeBytes (f : File) (buf : ByteArray) : IO Unit :=
  f.stream.write buf

/--
Flush a file.
-/
def flush (f : File) : IO Unit :=
  f.stream.flush

/-- Write to a raw fd number via FFI (throws on error).
    Kept because Lean's IO.FS.Stream.write does not surface
    ENOSPC (/dev/full) or EPIPE errors. -/
@[extern "lean_coreutils_write"]
opaque writeFd (fd : UInt32) (buf : @& ByteArray) : IO UInt32

/-- Convenience: write a ByteArray to stdout, throws on error. -/
def writeStdout (buf : ByteArray) : IO Unit := do
  let _ ← writeFd 1 buf
  pure ()

/-- Convenience: write a ByteArray to stderr, throws on error. -/
def writeStderr (buf : ByteArray) : IO Unit := do
  let _ ← writeFd 2 buf
  pure ()

/-- Convenience: write a String to stdout, throws on error. -/
def printOut (s : String) : IO Unit :=
  writeStdout s.toUTF8

/-- Convenience: write a String to stderr, throws on error. -/
def printErr (s : String) : IO Unit :=
  writeStderr s.toUTF8

/-- Convenience: read all bytes from stdin. -/
def readStdin : IO ByteArray := do
  let f ← stdin
  readAll f

/-- Ignore SIGPIPE so writes to closed pipes return EPIPE instead of crashing. -/
@[extern "lean_coreutils_ignore_sigpipe"]
opaque ignoreSigpipe : IO Unit

end Lentils.Common.IO.Native
