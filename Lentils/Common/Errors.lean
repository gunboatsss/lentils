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
Convert common errno values to human-readable strings (GNU/POSIX style).
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

/-- Extract the errno number from an IO error's toString.
    The format is: "... (error code: NNN) ..." -/
def getErrno (s : String) : UInt32 :=
  let marker := "(error code: "
  let parts := s.splitOn marker
  match parts with
  | [] => 0
  | _ :: rest =>
    let after := rest.headD ""
    -- Collect digit characters up to ')'
    let rec takeDigits (chars : List Char) : List Char :=
      match chars with
      | [] => []
      | c :: cs => if c ≥ '0' ∧ c ≤ '9' then c :: takeDigits cs else []
    let digitChars := takeDigits after.toList
    let numStr := String.ofList digitChars
    match numStr.toNat? with
    | some n => n.toUInt32
    | none => 0

/-- Format an IO error message in GNU style: errnoToString lookup with proper capitalization.
    Strips the trailing "(error code: NNN)" and "  file: ..." suffixes.
    Falls back to extracting the message before "(error code:" if the errno is not recognized. -/
def formatIoError (s : String) : String :=
  let line := (s.splitOn "\n").headD s
  let errno := getErrno line
  -- errno 0 means no code could be extracted: fall back to the raw
  -- message instead of reporting the bogus "Success".
  if errno == 0 then
    let parts := line.splitOn " (error code:"
    parts.headD line |>.trimRight
  else
    let fromErrno := errnoToString errno
    -- If the error code gave a meaningful message, use it; otherwise extract the raw message
    if fromErrno.startsWith "Unknown error" then
      -- Extract the part before "(error code:"
      let parts := line.splitOn " (error code:"
      parts.headD line |>.trimRight
    else
      fromErrno

end Lentils.Common.Errors
