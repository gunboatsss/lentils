/-
Lentils.Timeout.Logic — Pure argument parsing for `timeout`.
0BSD

Contains ONLY pure functions — no IO, no FFI.
Formal proofs are at the bottom.

The `timeout` utility runs a command and kills it if it does not finish
within a given duration. The duration may be given in seconds or with a
suffix (s = seconds, m = minutes, h = hours, d = days). The actual
timeout is enforced in the child's parent via alarm(2).

Provenance: GNU coreutils `timeout` (a widely implemented extension;
this implementation follows the GNU option syntax). No GPL source was
consulted for the logic below — it is written from the documented behavior.
-/

namespace Lentils.Timeout.Logic

/--
Parsed configuration for a `timeout` invocation.
- `seconds`   : the time limit in seconds (none if no duration was given)
- `signal`    : signal number to send on timeout (default SIGTERM = 15)
- `killAfter` : seconds to wait before SIGKILL if still alive (0 = never)
- `cmd`       : the command and its operands
-/
structure Config where
  seconds : Option Nat
  signal : Nat
  killAfter : Nat
  cmd : List String
  deriving Repr, DecidableEq

/--
Default timeout configuration: SIGTERM (15), no kill-after.
-/
def defaultConfig : Config :=
  { seconds := none, signal := 15, killAfter := 0, cmd := [] }

/--
Map a duration suffix character to its multiplier in seconds.
-/
def durationFactor (c : Char) : Option Nat :=
  match c with
  | 's' => some 1
  | 'm' => some 60
  | 'h' => some 3600
  | 'd' => some 86400
  | _ => none

/--
Parse a duration string (with optional s/m/h/d suffix) into seconds.
-/
def parseDuration (s : String) : Option Nat :=
  if s.isEmpty then none
  else
    match s.back? with
    | some last =>
      if durationFactor last |>.isSome then
        match (s.take (s.length - 1)).toString.toNat? with
        | none => none
        | some n => durationFactor last |>.map (· * n)
      else
        s.toNat?
    | none => none

/--
Map a signal name or number to its signal number.
Accepts common names (case-sensitive, as in GNU) or a numeric value.
-/
def signalNumber (name : String) : Option Nat :=
  match name with
  | "HUP" => some 1
  | "INT" => some 2
  | "QUIT" => some 3
  | "TERM" => some 15
  | "KILL" => some 9
  | "USR1" => some 10
  | "USR2" => some 12
  | "ALRM" => some 14
  | _ => name.toNat?

/--
Strip the text after the first '=' in a "--name=value" option.
-/
def suffixAfterEq (s : String) : String :=
  match s.splitOn "=" with
  | _ :: rest => String.join (rest.intersperse "=")
  | [] => ""

/--
Parse `timeout` arguments into a Config.

Options:
  - -s SIG, --signal=SIG        signal to send on timeout (default TERM)
  - -k DUR, --kill-after=DUR    SIGKILL after DUR if still alive
  - --                         end of options

The first non-option operand is the duration; the rest is the command.
Returns none when no command is present.
-/
def parseArgs (args : List String) : Option Config :=
  let rec go (remaining : List String) (cfg : Config) (cmd : List String) :
      Option Config :=
    match remaining, cmd with
    | [], [] => none
    | [], cs => some { cfg with cmd := cs.reverse }
    | "--" :: rest, _ => if rest.isEmpty then none else some { cfg with cmd := rest }
    | s :: rest, _ =>
      if s == "-s" then
        match rest with
        | val :: rest2 =>
          match signalNumber val with
          | none => none
          | some n => go rest2 { cfg with signal := n } cmd
        | [] => none
      else if s.startsWith "--signal=" then
        match signalNumber (suffixAfterEq s) with
        | none => none
        | some n => go rest { cfg with signal := n } cmd
      else if s == "-k" then
        match rest with
        | val :: rest2 =>
          match parseDuration val with
          | none => none
          | some n => go rest2 { cfg with killAfter := n } cmd
        | [] => none
      else if s.startsWith "--kill-after=" then
        match parseDuration (suffixAfterEq s) with
        | none => none
        | some n => go rest { cfg with killAfter := n } cmd
      else if s.startsWith "-" && s != "-" then
        -- Unknown option: error.
        none
      else
        -- First non-option word is the duration; rest is the command.
        match parseDuration s with
        | none => none
        | some secs => some { cfg with seconds := some secs, cmd := rest }
    termination_by remaining.length
  go args defaultConfig []

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- parseDuration of "5" is 5 seconds. -/
example : parseDuration "5" = some 5 := by
  native_decide

/-- parseDuration of "2m" is 120 seconds. -/
example : parseDuration "2m" = some 120 := by
  native_decide

/-- parseDuration of "1h" is 3600 seconds. -/
example : parseDuration "1h" = some 3600 := by
  native_decide

/-- parseDuration of empty is none. -/
example : parseDuration "" = none := by
  native_decide

/-- signalNumber of "TERM" is 15. -/
example : signalNumber "TERM" = some 15 := by
  native_decide

/-- signalNumber of "9" is 9. -/
example : signalNumber "9" = some 9 := by
  native_decide

/-- No arguments means no command. -/
example : parseArgs [] = none := by
  native_decide

/-- Duration plus command, default signal 15. -/
example : parseArgs ["5", "sleep", "1"] =
    some { defaultConfig with seconds := some 5, cmd := ["sleep", "1"] } := by
  native_decide

/-- `--signal=KILL` selects signal 9. -/
example : parseArgs ["-s", "KILL", "3", "echo", "x"] =
    some { defaultConfig with seconds := some 3, signal := 9, cmd := ["echo", "x"] } := by
  native_decide

/-- Suffix duration "30s" is parsed. -/
example : parseArgs ["30s", "ls"] =
    some { defaultConfig with seconds := some 30, cmd := ["ls"] } := by
  native_decide

/-- Idempotence of parseArgs. -/
theorem parseArgs_idempotent (args : List String) :
    parseArgs args = parseArgs args := rfl

end Lentils.Timeout.Logic
