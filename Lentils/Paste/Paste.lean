/-
Paste — IO wrapper for the `paste` utility. 0BSD -/
import Lentils.Paste.Logic

namespace Lentils.Paste

open Logic

def parseDelim (args : List String) : String :=
  if args.contains "-d" then
    match args with
    | "-d" :: d :: _ => d
    | _ => "\t"
  else
    -- Check for combined flags like "-d,"
    match args.find? (·.startsWith "-d") with
    | some a =>
      let rest := (a.drop 2).toString
      if rest == "" then "\t" else rest
    | none => "\t"

def isSerial (args : List String) : Bool :=
  args.any (· = "-s")

/-- Split a string into lines, dropping any trailing empty line from a final newline. -/
def splitLines (s : String) : List String :=
  let lines := s.splitOn "\n"
  match lines with
  | [] => []
  | _ :: _ =>
    -- If the last element is empty (from a trailing newline), drop it
    if lines.reverse.head? = some "" then
      (lines.reverse.tail).reverse
    else lines

/-- Read a file and split into lines, dropping any trailing empty line. -/
def readFileLines (path : String) : IO (List String) := do
  let content ←
    try IO.FS.readFile path
    catch _ => pure ""
  pure (splitLines content)

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
    fileLines := [splitLines content]
  let result :=
    if serial then pasteSerial fileLines delim
    else paste fileLines delim
  if !result.isEmpty then
    IO.print result
    IO.print "\n"
  return 0

end Lentils.Paste
