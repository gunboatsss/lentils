/-
Whoami — IO wrapper for the `whoami` utility. 0BSD -/
import Lentils.Whoami.Logic

namespace Lentils.Whoami

def run (_args : List String) : IO UInt32 := do
  let name ←
    match (← IO.getEnv "USER") with
    | some n => pure n
    | none =>
      match (← IO.getEnv "LOGNAME") with
      | some n => pure n
      | none =>
        match (← IO.getEnv "USERNAME") with
        | some n => pure n
        | none => pure ""
  if name.isEmpty then
    IO.eprintln "whoami: cannot find username"
    return 1
  else
    IO.println name
    return 0

end Lentils.Whoami
