/-
Logname — IO wrapper for the `logname` utility. 0BSD -/
import Lentils.Logname.Logic

namespace Lentils.Logname

def run (_args : List String) : IO UInt32 := do
  let name ←
    match (← IO.getEnv "LOGNAME") with
    | some n => pure n
    | none =>
      match (← IO.getEnv "USER") with
      | some n => pure n
      | none =>
        match (← IO.getEnv "USERNAME") with
        | some n => pure n
        | none => pure ""
  if name.isEmpty then
    IO.eprintln "logname: no login name"
    return 1
  else
    IO.println name
    return 0

end Lentils.Logname
