/-
Kill.Logic — Pure signal-number/mapping logic for `kill`. 0BSD

Contains ONLY pure functions: signal name ↔ number mapping,
argument parsing for `kill [-s signal] pid...` and `kill -l`.
No IO is performed here. All FFI lives in Kill.lean.
-/

namespace Lentils.Kill.Logic

/-- Parse error type for kill argument parsing. -/
inductive KillError where
  | invalidSignal (name : String)
  | missingArg (opt : String)
  deriving Repr, DecidableEq

/--
Map a signal name (case-insensitive) to its POSIX signal number.
Returns `some n` for recognized names, `none` otherwise.
Supports the full set of Linux/POSIX signal names.
-/
def signalNumber (name : String) : Option Int :=
  match name.trimAscii.toString.toUpper with
  | "HUP"    => some 1
  | "INT"    => some 2
  | "QUIT"   => some 3
  | "ILL"    => some 4
  | "TRAP"   => some 5
  | "ABRT"   => some 6
  | "IOT"    => some 6
  | "BUS"    => some 7
  | "FPE"    => some 8
  | "KILL"   => some 9
  | "USR1"   => some 10
  | "SEGV"   => some 11
  | "USR2"   => some 12
  | "PIPE"   => some 13
  | "ALRM"   => some 14
  | "TERM"   => some 15
  | "STKFLT" => some 16
  | "CHLD"   => some 17
  | "CLD"    => some 17
  | "CONT"   => some 18
  | "STOP"   => some 19
  | "TSTP"   => some 20
  | "TTIN"   => some 21
  | "TTOU"   => some 22
  | "URG"    => some 23
  | "XCPU"   => some 24
  | "XFSZ"   => some 25
  | "VTALRM" => some 26
  | "PROF"   => some 27
  | "WINCH"  => some 28
  | "IO"     => some 29
  | "POLL"   => some 29
  | "PWR"    => some 30
  | "SYS"    => some 31
  | _        => none

/--
Map a signal number to its canonical name.
Returns some name for 1-31, none otherwise.
-/
def signalName (n : Int) : Option String :=
  match n with
  | 1  => some "HUP"
  | 2  => some "INT"
  | 3  => some "QUIT"
  | 4  => some "ILL"
  | 5  => some "TRAP"
  | 6  => some "ABRT"
  | 7  => some "BUS"
  | 8  => some "FPE"
  | 9  => some "KILL"
  | 10 => some "USR1"
  | 11 => some "SEGV"
  | 12 => some "USR2"
  | 13 => some "PIPE"
  | 14 => some "ALRM"
  | 15 => some "TERM"
  | 16 => some "STKFLT"
  | 17 => some "CHLD"
  | 18 => some "CONT"
  | 19 => some "STOP"
  | 20 => some "TSTP"
  | 21 => some "TTIN"
  | 22 => some "TTOU"
  | 23 => some "URG"
  | 24 => some "XCPU"
  | 25 => some "XFSZ"
  | 26 => some "VTALRM"
  | 27 => some "PROF"
  | 28 => some "WINCH"
  | 29 => some "IO"
  | 30 => some "PWR"
  | 31 => some "SYS"
  | _  => none

/--
Parse a decimal string into an Int.
Returns none if the string is not a valid integer.
-/
def parseInt (s : String) : Option Int :=
  match s.trimAscii.toString.toInt? with
  | some n => some n
  | none => none

/--
Parse a signal specification:
- A bare number (e.g., "9")
- A signal name (e.g., "KILL", "TERM", "sigterm")
- A signal name prefixed with "SIG" (e.g., "SIGTERM")
Returns some signal number on success, none on failure.
-/
def parseSignal (s : String) : Option Int :=
  let trimmed := s.trimAscii.toString
  -- Try as bare number
  match trimmed.toInt? with
  | some n => if n >= 0 then some n else none
  | none =>
    let upper := trimmed.toUpper
    -- Strip "SIG" prefix if present
    let name := (if upper.startsWith "SIG" then upper.drop 3 else upper).toString
    signalNumber name

/--
Parse a pid string into a non-negative integer.
Returns none if invalid.
-/
def parsePid (s : String) : Option Int :=
  match s.trimAscii.toString.toInt? with
  | some n => if n >= 0 then some n else none
  | none => none

/--
Return the list of all signal names (1-31) formatted for `kill -l`.
Each entry is "N NAME".
-/
def listSignals : List String :=
  List.range 31 |>.map (λ (i : Nat) =>
    let n : Int := (i : Int) + (1 : Int)
    let name := (signalName n).getD "?"
    let nStr := toString n
    let padding := if n < (10 : Int) then " " else ""
    s!"{padding}{nStr} {name}"
  )

/--
Format a single `kill -l exit_status` result.
POSIX: when given an exit status > 128, subtract 128 and print the signal name.
For other positive integers, print the signal name (if valid) or number.
-/
def formatExitStatus (n : Int) : String :=
  let sigNum := if n > (128 : Int) then n - (128 : Int) else n
  match signalName sigNum with
  | some name => name
  | none => toString sigNum

/--
Format multiple signal names for `kill -l` (space-separated).
Extracts the signal name (second word) from each "N NAME" entry.
-/
def formatSignalList : String :=
  listSignals.map (λ s =>
    let parts := s.splitOn " "
    match parts with
    | ["", _n, name] => name    -- single-digit signals: "" "n" "NAME"
    | [_n, name] => name        -- double-digit signals: "nn" "NAME"
    | _ => s
  ) |> String.intercalate " "

/--
The default signal sent by `kill` when none is specified.
-/
def defaultSignal : Int := 15  -- SIGTERM

/--
Parse the kill arguments to extract signal and pid list.
Handles:
  -s signal pid...
  --signal pid...
  -signal pid...
  pid... (default signal)
Returns an error for invalid signal names or missing option arguments.
-/
def parseKillArgs (args : List String) (defaultSig : Int) : Except KillError (Int × List String) :=
  let rec go (remaining : List String) (sig : Option Int) (pids : List String) : Except KillError (Int × List String) :=
    match remaining with
    | [] =>
      Except.ok (sig.getD defaultSig, pids.reverse)
    | "-s" :: s :: rest =>
      match parseSignal s with
      | some n => go rest (some n) pids
      | none   => Except.error (KillError.invalidSignal s)
    | "--signal" :: s :: rest =>
      match parseSignal s with
      | some n => go rest (some n) pids
      | none   => Except.error (KillError.invalidSignal s)
    | "-s" :: [] =>
      Except.error (KillError.missingArg "-s")
    | "--signal" :: [] =>
      Except.error (KillError.missingArg "--signal")
    | arg :: rest =>
      if arg.startsWith "-" && arg.length > 1 then
        let stripped := arg.drop 1
        match stripped.toString.toInt? with
        | some n => go rest (some n) pids
        | none =>
          match parseSignal (stripped.toString) with
          | some n => go rest (some n) pids
          | none =>
            go rest sig (pids ++ [arg])
      else
        go rest sig (pids ++ [arg])
  go args none []

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- SIGTERM is signal 15. -/
example : signalNumber "TERM" = some 15 := by
  native_decide

/-- SIGKILL is signal 9. -/
example : signalNumber "KILL" = some 9 := by
  native_decide

/-- SIGKILL (lowercase) works. -/
example : signalNumber "kill" = some 9 := by
  native_decide

/-- SIGHUP (case-insensitive) works. -/
example : signalNumber "Hup" = some 1 := by
  native_decide

/-- SIG prefix is handled. -/
example : parseSignal "SIGTERM" = some 15 := by
  native_decide

/-- Bare signal number as string. -/
example : parseSignal "9" = some 9 := by
  native_decide

/-- Signal 15 maps to TERM. -/
example : signalName 15 = some "TERM" := by
  native_decide

/-- Signal 9 maps to KILL. -/
example : signalName 9 = some "KILL" := by
  native_decide

/-- Invalid signal name returns none. -/
example : signalNumber "BOGUS" = none := by
  native_decide

/-- Exit status 143 → 143-128 = 15 (SIGTERM). -/
example : formatExitStatus 143 = "TERM" := by
  native_decide

/-- Exit status 137 → 137-128 = 9 (SIGKILL). -/
example : formatExitStatus 137 = "KILL" := by
  native_decide

/-- Empty string parse to none. -/
example : parseSignal "" = none := by
  native_decide

/-- Default signal is SIGTERM (15). -/
example : defaultSignal = 15 := by
  native_decide

/-- formatSignalList first name is HUP. -/
example : (formatSignalList.splitOn " ").head? = some "HUP" := by
  native_decide

end Lentils.Kill.Logic
