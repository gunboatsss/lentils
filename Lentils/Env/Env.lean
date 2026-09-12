/-
Env — IO wrapper for the `env` utility. 0BSD -/
import Lentils.Env.Logic

namespace Lentils.Env

open Logic

/-- FFI: list all environment variables as "KEY=VALUE" strings. -/
@[extern "lean_coreutils_list_env"]
opaque listEnv : IO (List String)

/-- FFI: run a command with modified environment. -/
@[extern "lean_coreutils_run_env"]
opaque runEnv (envVars : Array String) (clearEnv : UInt32) (cmdArgs : Array String) : IO UInt32

def run (args : List String) : IO UInt32 := do
  let parsed := parseArgs { args := args }
  if parsed.cmdArgs.isEmpty then
    -- No command: print environment (possibly modified)
    if parsed.clearEnv && parsed.envPairs.isEmpty then
      -- -i with no vars: empty environment
      return 0
    else if parsed.clearEnv then
      -- -i with vars: only print the vars provided
      for v in parsed.envPairs do
        IO.println v
      return 0
    else
      -- Print current environment (vars are set in the child, not here)
      let env ← listEnv
      for entry in env do
        IO.println entry
      return 0
  else
    -- Run command with modified environment
    try
      let envVarsArr := parsed.envPairs.toArray
      let cmdArgsArr := parsed.cmdArgs.toArray
      let exitCode ← runEnv envVarsArr (if parsed.clearEnv then 1 else 0) cmdArgsArr
      return exitCode
    catch _ =>
      IO.eprintln "env: failed to execute command"
      return 127

end Lentils.Env
