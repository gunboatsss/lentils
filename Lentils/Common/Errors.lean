/-
Lentils.Common.Errors — Error reporting helpers.
0BSD

Produces POSIX-style error messages on stderr and returns
non-zero exit codes. All functions use IO for output but
the message formatting is pure.

Provenance: implemented from POSIX.1-2017 error message conventions.
No GPL source was consulted.
-/

namespace Lentils.Common.Errors

/--
Format an error message in the standard `program: file: message` style.
When `file?` is none, produces `program: message`.
-/
def errorMessage (prog : String) (file? : Option String) (msg : String) : String :=
  match file? with
  | none   => s!"{prog}: {msg}"
  | some f => s!"{prog}: {f}: {msg}"

/--
Print an error message to stderr and return exit code 1.
-/
def exitError (prog : String) (file? : Option String) (msg : String) : IO UInt32 := do
  IO.eprintln (errorMessage prog file? msg)
  return 1

/--
Print a usage message to stderr matching GNU coreutils format:
  prog: missing file operand
  Try 'prog --help' for more information.
Returns exit code 1 (GNU convention).
-/
def exitUsage (prog : String) (argMsg : String) : IO UInt32 := do
  IO.eprintln s!"{prog}: {argMsg}"
  IO.eprintln s!"Try '{prog} --help' for more information."
  return 1

/--
Convert common errno values to human-readable strings.
This is a pure fallback when FFI strerror is not available.
-/
def errnoToString (err : UInt32) : String :=
  match err with
  | 0  => "Success"
  | 1  => "Operation not permitted"
  | 2  => "No such file or directory"
  | 3  => "No such process"
  | 4  => "Interrupted system call"
  | 5  => "I/O error"
  | 6  => "No such device or address"
  | 9  => "Bad file descriptor"
  | 11 => "Resource temporarily unavailable"
  | 12 => "Cannot allocate memory"
  | 13 => "Permission denied"
  | 17 => "File exists"
  | 20 => "Not a directory"
  | 21 => "Is a directory"
  | 27 => "File too large"
  | 28 => "No space left on device"
  | 32 => "Broken pipe"
  | _  => s!"Unknown error {err}"

end Lentils.Common.Errors
