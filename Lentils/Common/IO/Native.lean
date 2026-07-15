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

/--
Convenience: read stdin as a String.
Returns empty string on error (e.g. no stdin).
-/
def readStdinText : IO String := do
  try IO.FS.readFile "/dev/stdin"
  catch _ => pure ""

/--
Read stdin as lines, stripping the trailing empty line
that results from a final newline.
Returns [] on empty input.
-/
def readStdinLines : IO (List String) := do
  let input ← readStdinText
  let lines := input.splitOn "\n"
  pure (if lines.length > 0 then
    let last := lines.length - 1
    match lines.drop last with
    | [""] => lines.take last
    | _ => lines
  else lines)

/-- Ignore SIGPIPE so writes to closed pipes return EPIPE instead of crashing. -/
@[extern "lean_coreutils_ignore_sigpipe"]
opaque ignoreSigpipe : IO Unit

/-- unlink(2): remove a directory entry (file or symlink). Throws on failure. -/
@[extern "lean_coreutils_unlink"]
opaque unlink (path : String) : IO Unit

/-- rmdir(2): remove an empty directory. Throws on failure. -/
@[extern "lean_coreutils_rmdir"]
opaque rmdir (path : String) : IO Unit

/-- symlink(2): create a symlink `linkpath` -> `target`. Throws on failure. -/
@[extern "lean_coreutils_symlink"]
opaque symlink (target : String) (linkpath : String) : IO Unit

/-- link(2): create a hard link `newpath` -> `oldpath`. Throws on failure. -/
@[extern "lean_coreutils_link"]
opaque link (oldpath : String) (newpath : String) : IO Unit

/-- chmod(2): change permission bits of `path` to `mode`. Throws on failure. -/
@[extern "lean_coreutils_chmod"]
opaque chmod (path : String) (mode : UInt32) : IO Unit

/-- stat(2): return st_mode permission bits of `path`. Throws on failure. -/
@[extern "lean_coreutils_stat_mode"]
opaque statMode (path : String) : IO UInt32

/-- gettimeofday(2): return current time as (seconds, microseconds) packed into UInt64.
    The top 32 bits are microseconds, the bottom 32 bits are seconds. -/
@[extern "lean_coreutils_gettimeofday"]
opaque gettimeofday : IO UInt64

/-- stat(2): return all stat fields as an Array of UInt64:
    [mode, size, nlink, uid, gid, blocks, blksize, dev, ino, rdev].
    Throws on failure. -/
@[extern "lean_coreutils_stat_all"]
opaque statAll (path : String) : IO (Array UInt64)

/-- statvfs(2): return filesystem info as an Array of UInt64:
    [f_bsize, f_frsize, f_blocks, f_bfree, f_bavail, f_files, f_ffree, f_favail, f_namemax].
    Throws on failure. -/
@[extern "lean_coreutils_statvfs_all"]
opaque statvfsAll (path : String) : IO (Array UInt64)

/-- truncate(2): truncate a file to a given size. Fails on non-existent files. -/
@[extern "lean_coreutils_truncate"]
opaque truncate (path : String) (size : UInt64) : IO Unit

/-- truncate with file creation (open+ftruncate): creates non-existent files. -/
@[extern "lean_coreutils_truncate_file"]
opaque truncateFile (path : String) (size : UInt64) : IO Unit

/-- lstat(2): same as statAll but uses lstat (does not follow symlinks). -/
@[extern "lean_coreutils_lstat_all"]
opaque lstatAll (path : String) : IO (Array UInt64)

/-- getmntent(3): return list of mounted filesystem paths as an Array of strings. -/
@[extern "lean_coreutils_getmounts"]
opaque getMounts : IO (Array String)

/-- mkfifo(3): create a FIFO (named pipe) with given path and mode.
    Throws on failure. -/
@[extern "lean_coreutils_mkfifo"]
opaque mkfifo (path : String) (mode : UInt32) : IO Unit

/-- mknod(2): create a block or character special file.
    Throws on failure. -/
@[extern "lean_coreutils_mknod"]
opaque mknod (path : String) (mode : UInt32) (major : UInt32) (minor : UInt32) : IO Unit

/-- chown(2): change owner and/or group of a file.
    Empty string for owner or group means "don't change" that component.
    Throws on failure. -/
@[extern "lean_coreutils_chown"]
opaque chown (path : String) (owner : String) (group : String) : IO Unit

/-- sync(2): flush all filesystem buffers to disk. -/
@[extern "lean_coreutils_sync"]
opaque sync : IO Unit

/-- mkdtemp(3): create a temporary directory.
    Template string is modified in-place; returns the actual name.
    Throws on failure. -/
@[extern "lean_coreutils_mkdtemp"]
opaque mkdtemp (template : String) : IO String

/-- mkstemp(3): create a temporary file.
    Template string is modified in-place; returns the actual name.
    Throws on failure. -/
@[extern "lean_coreutils_mkstemp"]
opaque mkstemp (template : String) : IO String

/-- open_wronly(2): open file for writing without truncation. Returns fd. -/
@[extern "lean_coreutils_open_wronly"]
opaque openWronly (path : String) : IO UInt32

/-- close(2): close a file descriptor. -/
@[extern "lean_coreutils_close"]
opaque close (fd : UInt32) : IO Unit

/-- fsync(2): synchronize file with storage device. -/
@[extern "lean_coreutils_fsync"]
opaque fsync (fd : UInt32) : IO Unit

/-- lseek(2): seek to position in file. Returns new offset. -/
@[extern "lean_coreutils_lseek"]
opaque lseek (fd : UInt32) (offset : Int64) (whence : UInt32) : IO UInt64

/-- getpwuid_gecos(3): return GECOS (real name) for a UID.
    Returns empty string if not found. -/
@[extern "lean_coreutils_getpwuid_gecos"]
opaque getpwuidGecos (uid : UInt32) : IO String

/-- isatty(3): check if a file descriptor refers to a terminal.
    Returns 1 if yes, 0 if no. -/
@[extern "lean_coreutils_isatty"]
opaque isatty (fd : UInt32) : IO UInt32

/-- ttyname(3): return the terminal device path for a file descriptor.
    Returns empty string if not a tty. -/
@[extern "lean_coreutils_ttyname"]
opaque ttyname (fd : UInt32) : IO String

end Lentils.Common.IO.Native
