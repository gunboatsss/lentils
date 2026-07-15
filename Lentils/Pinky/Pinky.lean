/-
Pinky — IO wrapper for the `pinky` utility. 0BSD
-/

import Lentils.Pinky.Logic
import Lentils.Common.IO.Native

namespace Lentils.Pinky

open Logic
open Lentils.Common.IO.Native

@[extern "lean_coreutils_who"]
opaque getWhoEntries : IO (Array String)

@[extern "lean_coreutils_getpwnam"]
opaque getpwnam (name : String) : IO String

def run (args : List String) : IO UInt32 := do
  let longFormat := args.any (· = "-l")
  let rawEntries ← getWhoEntries

  let mut entries : List Entry := []
  for raw in rawEntries do
    match parseWhoEntry raw with
    | none => pure ()
    | some (user, line, timeStr, host, state, idleSecs) =>
      let uidStr ← getpwnam user
      let realName ←
        if uidStr.isEmpty then pure ""
        else
          let parts := uidStr.splitOn ":"
          if parts.length > 0 then
            match parts[0]!.toNat? with
            | some uid => getpwuidGecos (UInt32.ofNat uid)
            | none => pure ""
          else pure ""
      entries := { user, realName, line, timeStr, host, state, idleSecs } :: entries

  if entries.isEmpty then
    return 0

  let output :=
    if longFormat then
      String.intercalate "\n" (entries.reverse.map (λ e =>
        let hostStr := if e.host.isEmpty then "" else " from " ++ e.host
        let stateStr := if e.state = "?" then "" else " (" ++ e.state ++ ")"
        "Login: " ++ e.user ++ "  Name: " ++ e.realName ++ "\n" ++
        "On since " ++ e.timeStr ++ " on " ++ e.line ++ stateStr ++ hostStr))
    else
      formatShort entries.reverse

  IO.println output
  return 0

end Lentils.Pinky