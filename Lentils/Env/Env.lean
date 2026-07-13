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
  let (clearEnv, envVars, cmdArgs) := parseArgs args
  if cmdArgs.isEmpty then
    -- No command: print environment (possibly modified)
    if clearEnv && envVars.isEmpty then
      -- -i with no vars: empty environment
      return 0
    else if clearEnv then
      -- -i with vars: only print the vars provided
      for v in envVars do
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
      let envVarsArr := envVars.toArray
      let cmdArgsArr := cmdArgs.toArray
      let exitCode ← runEnv envVarsArr (if clearEnv then 1 else 0) cmdArgsArr
      return exitCode
    catch _ =>
      IO.eprintln "env: failed to execute command"
      return 127

end Lentils.Env
