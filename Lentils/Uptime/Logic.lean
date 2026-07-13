/-
Uptime.Logic — Pure uptime formatting for `uptime`. 0BSD -/
import Lentils.Common.Float

namespace Lentils.Uptime.Logic

def parseUptime (content : String) : Float :=
  match content.splitOn " " with
  | [] => 0.0
  | secs :: _ =>
    let trimmed := secs.trimAscii.toString
    if trimmed.isEmpty then 0.0
    else
      match Lentils.Common.Float.parse trimmed with
      | some f => f
      | none => 0.0

def formatUptime (seconds : Float) : String :=
  let total := seconds.toUInt64
  let days := total / 86400
  let hours := (total % 86400) / 3600
  let minutes := (total % 3600) / 60
  if days > 0 then
    s!"{days} day{if days > 1 then "s" else ""}, {hours} hour{if hours > 1 && hours != 0 then "s" else ""}, {minutes} minute{if minutes > 1 && minutes != 0 then "s" else ""}"
  else if hours > 0 then
    s!"{hours} hour{if hours > 1 then "s" else ""}, {minutes} minute{if minutes > 1 then "s" else ""}"
  else
    s!"{minutes} minute{if minutes != 1 then "s" else ""}"

end Lentils.Uptime.Logic
