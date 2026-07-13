/-
Printenv — IO wrapper for the `printenv` utility. 0BSD -/
import Lentils.Printenv.Logic

namespace Lentils.Printenv

open Logic

/-- FFI: get all environment variables as Array String ("KEY=VALUE" entries). -/
@[extern "lean_coreutils_environ"]
opaque getEnviron : IO (Array String)

def run (args : List String) : IO UInt32 := do
  let nonFlagArgs := args.filter (λ a => !a.startsWith "-")
  if nonFlagArgs.isEmpty then
    -- Print all environment variables
    let env ← getEnviron
    for entry in env do
      IO.println entry
    return 0
  else
    -- Print specific variables
    let allEnv ← getEnviron
    let mut foundAny := false
    for name in nonFlagArgs do
      let matching := allEnv.filter (λ e => matchesVar e name)
      if matching.isEmpty then
        foundAny := true  -- but we still exit 1 if any are missing
      else
        for entry in matching do
          IO.println (extractValue entry)
          foundAny := true
    if foundAny then
      -- Check if any requested variable was not found
      let allFound := nonFlagArgs.all (λ name =>
        allEnv.any (λ e => matchesVar e name))
      return if allFound then 0 else 1
    else
      return 1

end Lentils.Printenv
