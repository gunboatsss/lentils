/-
Comm — IO wrapper for the `comm` utility. 0BSD -/
import Lentils.Comm.Logic

namespace Lentils.Comm

open Logic

/-- Split a string into lines, dropping any trailing empty line from a final newline. -/
def splitLines (s : String) : List String :=
  let lines := s.splitOn "\n"
  match lines with
  | [] => []
  | _ :: _ =>
    if lines.reverse.head? = some "" then
      (lines.reverse.tail).reverse
    else lines

/-- Check if a flag string (e.g. "-1", "-123", "-23") contains the given digit. -/
def hasFlag (flag : String) (digit : Char) : Bool :=
  flag.startsWith "-" && flag.contains digit

/-- Parse flags from arguments, supporting combined flags like "-123". -/
def parseFlags (args : List String) : SuppressFlags :=
  let col1 := args.any fun a => hasFlag a '1'
  let col2 := args.any fun a => hasFlag a '2'
  let col3 := args.any fun a => hasFlag a '3'
  { col1, col2, col3 }


/-- Return true if the argument is a flag (starts with "-" and is not "-"). -/
def isFlag (a : String) : Bool :=
  a.startsWith "-" && a ≠ "-"

def run (args : List String) : IO UInt32 := do
  let flags := parseFlags args
  let files := args.filter (λ a => !isFlag a)
  let mut anyError := false
  let lines1 ←
    match files with
    | f1 :: _ =>
      if f1 = "-" then
        try
          let content ← IO.FS.readFile "/dev/stdin"
          pure (splitLines content)
        catch _ =>
          anyError := true
          pure []
      else
        try
          let content ← IO.FS.readFile f1
          pure (splitLines content)
        catch _ =>
          IO.eprintln s!"comm: {f1}: No such file or directory"
          anyError := true
          pure []
    | [] => pure []
  let lines2 ←
    match files with
    | _ :: f2 :: _ =>
      if f2 = "-" then
        try
          let content ← IO.FS.readFile "/dev/stdin"
          pure (splitLines content)
        catch _ =>
          anyError := true
          pure []
      else
        try
          let content ← IO.FS.readFile f2
          pure (splitLines content)
        catch _ =>
          IO.eprintln s!"comm: {f2}: No such file or directory"
          anyError := true
          pure []
    | _ => pure []
  if anyError then
    return 1
  let result := comm lines1 lines2 flags
  if !result.isEmpty then
    IO.print result
    IO.print "\n"
  return 0

end Lentils.Comm
