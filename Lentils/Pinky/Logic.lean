namespace Lentils.Pinky.Logic

/-- A pinky entry. -/
structure Entry where
  user : String
  realName : String
  line : String
  timeStr : String
  host : String
  state : String   -- "+", "-", or "?"
  idleSecs : String
  deriving Repr

/-- Parse a raw who entry into (user, line, time, host, state, idleSecs). -/
def parseWhoEntry (raw : String) : Option (String × String × String × String × String × String) :=
  let parts := raw.splitOn "|"
  match parts with
  | user :: line :: timeStr :: host :: state :: idleSecs :: _ =>
    some (user, line, timeStr, host, state, idleSecs)
  | _ => none

/-- Pad or truncate a string to exactly n characters. -/
def pad (s : String) (n : Nat) : String :=
  let cs := s.toList
  if cs.length >= n then String.ofList (cs.take n)
  else s ++ String.ofList (List.replicate (n - cs.length) ' ')

/-- Format idle seconds: ".", "mm", "hh:mm", or "Ndays". -/
def formatIdle (secsStr : String) : String :=
  match secsStr.toNat? with
  | none => "?"
  | some secs =>
    if secs < 60 then "."
    else if secs < 3600 then
      let m := secs / 60
      if m < 10 then " " ++ toString m ++ "m" else toString m ++ "m"
    else if secs < 86400 then
      let h := secs / 3600
      let m := (secs % 3600) / 60
      let hStr := if h < 10 then " " ++ toString h else toString h
      let mStr := if m < 10 then "0" ++ toString m else toString m
      hStr ++ ":" ++ mStr
    else
      let days := secs / 86400
      toString days ++ "d"

/-- Short format with header: Login, Name, TTY, Idle, When, Where. -/
def formatShort (entries : List Entry) : String :=
  let header := "Login    Name                 TTY      Idle   When             Where"
  let lines := entries.map (λ e =>
    let userPad := pad e.user 8
    let namePad := pad e.realName 20
    let ttyField := e.state ++ pad e.line 5
    let idleField := pad (formatIdle e.idleSecs) 6
    let timeField := pad e.timeStr 16
    let whereField := if e.host.isEmpty then "" else e.host
    userPad ++ " " ++ namePad ++ " " ++ ttyField ++ " " ++ idleField ++ " " ++
      timeField ++ " " ++ whereField)
  String.intercalate "\n" (header :: lines)

-- ─── Proofs ──────────────────────────────────────────────────────────────────


theorem pad_hello_3 : pad "hello" 3 = "hel" := by native_decide

theorem pad_hello_8 : pad "hello" 8 = "hello   " := by native_decide

theorem formatIdle_dot : formatIdle "30" = "." := by native_decide

theorem formatIdle_minutes : formatIdle "120" = " 2m" := by native_decide

theorem formatIdle_hours : formatIdle "3661" = " 1:01" := by native_decide

theorem formatIdle_days : formatIdle "90000" = "1d" := by native_decide

theorem formatIdle_unknown : formatIdle "abc" = "?" := by native_decide

theorem parseWhoEntry_full :
  parseWhoEntry "alice|tty1|2026-07-15 10:00|myhost|+|120|1234" =
    some ("alice", "tty1", "2026-07-15 10:00", "myhost", "+", "120") := by
  native_decide

theorem parseWhoEntry_short :
  parseWhoEntry "bob|pts/0|10:30|:0" = none := by native_decide

end Lentils.Pinky.Logic
