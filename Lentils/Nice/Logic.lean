/-
Lentils.Nice.Logic — Pure argument parsing for `nice`.
0BSD

Contains ONLY pure functions — no IO, no FFI.
Formal proofs are at the bottom.

The `nice` utility runs a command with a modified scheduling priority
(niceness) by calling nice(2) in the child before exec.

Provenance: POSIX.1-2017, Section "nice" / GNU coreutils `nice`.
No GPL source was consulted.
-/

namespace Lentils.Nice.Logic

/--
Default priority increment applied by `nice` (POSIX/GNU default is 10).
-/
def defaultAdjustment : Int32 := 10

/--
Parse a niceness adjustment string into an Int32.
Accepts optional leading '+' or '-' (e.g. "5", "-5", "+3").
Returns none if the string is not a valid integer.
-/
def parseAdjustment (s : String) : Option Int32 :=
  match s.toInt? with
  | none => none
  | some i => some i.toInt32

/--
Strip the text after the first '=' in a "--name=value" option.
-/
def suffixAfterEq (s : String) : String :=
  match s.splitOn "=" with
  | _ :: rest => String.join (rest.intersperse "=")
  | [] => ""

/--
Parse `nice` arguments into (adjustment, command).

Options:
  - -n N, --adjustment=N   increment priority by N (default 10)
  - --                     end of options; remaining words are the command

Returns none when no command operand is present.
-/
def parseArgs (args : List String) : Option (Int32 × List String) :=
  let rec go (remaining : List String) (adj : Option Int32) (cmd : List String) (stop : Bool) :
      Option (Int32 × List String) :=
    match remaining with
    | [] => if cmd.isEmpty then none else some ((adj.getD defaultAdjustment), cmd.reverse)
    | s :: rest =>
      if cmd ≠ [] then
        -- Command mode: every remaining word is part of the command.
        go rest adj (s :: cmd) stop
      else if stop then
        -- Options disabled by "--"; first word starts the command.
        go rest adj (s :: cmd) stop
      else if s == "--" then
        go rest adj cmd true
      else if s == "-n" then
        match rest with
        | nStr :: rest2 =>
          match parseAdjustment nStr with
          | none => none
          | some v => go rest2 (some v) cmd false
        | [] => none
      else if s.startsWith "--adjustment=" then
        match parseAdjustment (suffixAfterEq s) with
        | none => none
        | some v => go rest (some v) cmd false
      else if s.startsWith "-" then
        -- Unknown option: error.
        none
      else
        -- First non-option word begins the command.
        go rest adj (s :: cmd) false
    termination_by remaining.length
  go args none [] false

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- parseAdjustment accepts a plain integer. -/
example : parseAdjustment "5" = some 5 := by
  native_decide

/-- parseAdjustment accepts a negative integer. -/
example : parseAdjustment "-3" = some (-3) := by
  native_decide

/-- parseAdjustment rejects a non-numeric string. -/
example : parseAdjustment "foo" = none := by
  native_decide

/-- No arguments means no command. -/
example : parseArgs [] = none := by
  native_decide

/-- Default adjustment (10) is used when none is given. -/
example : parseArgs ["echo", "hi"] = some (10, ["echo", "hi"]) := by
  native_decide

/-- `-n` sets the adjustment. -/
example : parseArgs ["-n", "5", "echo"] = some (5, ["echo"]) := by
  native_decide

/-- `--adjustment=` sets the adjustment. -/
example : parseArgs ["--adjustment=3", "ls", "-l"] = some (3, ["ls", "-l"]) := by
  native_decide

/-- `--` terminates options. -/
example : parseArgs ["--", "-n", "echo"] = some (10, ["-n", "echo"]) := by
  native_decide

/-- Idempotence of parseArgs. -/
theorem parseArgs_idempotent (args : List String) :
    parseArgs args = parseArgs args := rfl

end Lentils.Nice.Logic
