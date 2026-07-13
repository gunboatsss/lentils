/-
Env.Logic — Pure environment variable parsing logic for `env`. 0BSD -/
namespace Lentils.Env.Logic

/--
Check if a string looks like an environment variable assignment.
-/
def isEnvAssignment (s : String) : Bool :=
  s.contains '='

/--
Parse env args into (clearEnv, envPairs, cmdArgs).
- `-i` or `--ignore-environment` clears the environment.
- `name=value` entries are environment variable assignments.
- Everything else is treated as command + args.
-/
partial def parseArgs (args : List String) : Bool × List String × List String :=
  let rec go (remaining : List String) (clearEnv : Bool) (envPairs : List String) :
      Bool × List String × List String :=
    match remaining with
    | [] => (clearEnv, envPairs.reverse, [])
    | "-i" :: rest => go rest true envPairs
    | "--ignore-environment" :: rest => go rest true envPairs
    | "-" :: rest => (clearEnv, envPairs.reverse, rest)
    | "--" :: rest => (clearEnv, envPairs.reverse, rest)
    | (s :: rest) =>
      if s.startsWith "-" then
        -- Unknown flag, treat rest as positional args (command)
        (clearEnv, envPairs.reverse, remaining)
      else if isEnvAssignment s then
        go rest clearEnv (s :: envPairs)
      else
        -- Start of command
        (clearEnv, envPairs.reverse, remaining)
  go args false []

end Lentils.Env.Logic
