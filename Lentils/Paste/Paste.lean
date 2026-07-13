/-
Paste — IO wrapper for the `paste` utility. 0BSD -/
import Lentils.Paste.Logic

namespace Lentils.Paste

open Logic

def parseDelim (args : List String) : String :=
  match args with
  | "-d" :: d :: _ => d
  | _ => "\t"

def isSerial (args : List String) : Bool :=
  args.any (· = "-s")

def readFileLines (path : String) : IO (List String) := do
  let content ←
    try IO.FS.readFile path
    catch _ => pure ""
  pure (content.splitOn "\n")

def run (args : List String) : IO UInt32 := do
  let delim := parseDelim args
  let serial := isSerial args
  let files := args.filter (λ a => !a.startsWith "-")
  let mut fileLines : List (List String) := []
  for file in files do
    let lines ← readFileLines file
    fileLines := fileLines ++ [lines]
  if fileLines.isEmpty then
    -- Read stdin
    let content ←
      try IO.FS.readFile "/dev/stdin"
      catch _ => pure ""
    fileLines := [content.splitOn "\n"]
  let result :=
    if serial then pasteSerial fileLines delim
    else paste fileLines delim
  IO.print result
  return 0

end Lentils.Paste
