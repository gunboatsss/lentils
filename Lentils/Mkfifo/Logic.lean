/-
Mkfifo.Logic — Pure logic for the `mkfifo` utility.
0BSD

Contains only pure functions: argument parsing. No IO is performed here.
All filesystem interaction lives in `mkfifo.lean` via the C FFI `mkfifo(3)`.

Provenance: POSIX.1-2017, Section "mkfifo — make FIFO special files".
No GPL source was consulted.
-/

namespace Lentils.Mkfifo.Logic

/--
Options controlling `mkfifo` behaviour.

-m MODE : set file mode (permissions) of the FIFO
-/
structure Options where
  mode : UInt32 := 0o666
  verbose : Bool := false
  deriving Repr, BEq, DecidableEq

/--
Parse a mode string (octal number) into a UInt32.
Returns 0o666 (octal) on parse failure.
-/
def parseMode (s : String) : UInt32 :=
  if s.isEmpty then 0o666 else
  let rec go (chars : List Char) (acc : UInt32) : UInt32 :=
    match chars with
    | [] => acc
    | c :: rest =>
      if c ≥ '0' && c ≤ '7' then
        go rest (acc * 8 + (UInt32.ofNat (c.toNat - 0x30)))
      else
        0o666  -- invalid octal digit
  go (s.toList) 0

/--
Parse `mkfifo` arguments into `(options, names)`.

  mkfifo [-m MODE] NAME...
-/
def parseArgs (args : List String) : Options × List String :=
  let rec go (remaining : List String) (opts : Options) (names : List String)
      : Options × List String :=
    match remaining with
    | [] => (opts, names.reverse)
    | "--" :: rest => (opts, names.reverse ++ rest)
    | "-m" :: modeStr :: rest => go rest { opts with mode := parseMode modeStr } names
    | "--mode" :: modeStr :: rest => go rest { opts with mode := parseMode modeStr } names
    | "-v" :: rest => go rest { opts with verbose := true } names
    | "--verbose" :: rest => go rest { opts with verbose := true } names
    | s :: rest =>
      if s.startsWith "-" && s != "-" then
        (opts, names.reverse)
      else
        go rest opts (s :: names)
  go args {} []

-- ─── Theorems ──────────────────────────────────────────────────────────────────

theorem parse_simple :
  (parseArgs ["fifo"]).2 = ["fifo"] := by native_decide

theorem parse_mode :
  (parseArgs ["-m", "644", "fifo"]).1.mode = 0o644 := by native_decide

/-- parseMode: empty string returns default (0o666). -/
theorem parseMode_empty :
  parseMode "" = 0o666 := by native_decide

/-- parseMode: octal 0 = 0. -/
theorem parseMode_zero :
  parseMode "0" = 0 := by native_decide

/-- parseMode: octal 644 = 420. -/
theorem parseMode_644 :
  parseMode "644" = 0o644 := by native_decide

/-- parseMode: octal 755 = 493. -/
theorem parseMode_755 :
  parseMode "755" = 0o755 := by native_decide

/-- parseMode: octal 777 = 511. -/
theorem parseMode_777 :
  parseMode "777" = 0o777 := by native_decide

/-- parseMode: invalid digit returns default. -/
theorem parseMode_invalid :
  parseMode "8" = 0o666 := by native_decide

/-- parseMode: letters return default. -/
theorem parseMode_alpha :
  parseMode "abc" = 0o666 := by native_decide

/-- parseMode: leading zeros are handled. -/
theorem parseMode_leading_zeros :
  parseMode "0755" = 0o755 := by native_decide

/-- Multiple names are collected. -/
theorem parse_multiple :
  (parseArgs ["a", "b", "c"]).2 = ["a", "b", "c"] := by native_decide

/-- Verbose flag is set. -/
theorem parse_verbose :
  (parseArgs ["-v", "fifo"]).1.verbose = true := by native_decide

end Lentils.Mkfifo.Logic
