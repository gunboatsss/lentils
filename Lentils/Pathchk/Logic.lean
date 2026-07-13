/-
Pathchk.Logic — Pure pathname validity checking for `pathchk`. 0BSD
-/

namespace Lentils.Pathchk.Logic

def maxComponentLen : Nat := 255
def maxPathLen : Nat := 4096

def portableMaxComponentLen : Nat := 14
def portableMaxPathLen : Nat := 256

def portableChars : List Char :=
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._-".toList

def isPortableChar (c : Char) : Bool :=
  portableChars.any (λ p => p == c)

/-- Enumerate list with indices. -/
def enumerate (xs : List String) : List (Nat × String) :=
  let rec go (i : Nat) (remaining : List String) : List (Nat × String) :=
    match remaining with
    | [] => []
    | x :: xs => (i, x) :: go (i + 1) xs
  go 0 xs

/-- Check if a single pathname component is valid.
    Empty component at start is ok (absolute path indicator).
    Returns none if valid, some error message if invalid. -/
def checkComponent (comp : String) (portable : Bool) (isFirst : Bool) (strict : Bool := false) : Option String :=
  if comp.contains (λ c => c == '\x00') then
    some "null byte in component"
  else if comp.isEmpty then
    if isFirst then
      none  -- leading "/" produces empty first component; that's ok
    else if strict then
      some "empty component"
    else
      none  -- non-strict allows empty components (consecutive slashes)
  else if strict && comp.startsWith "-" then
    some "leading dash"
  else if portable && comp.any (λ c => !isPortableChar c) then
    some "non-portable character"
  else
    let maxLen := if portable then portableMaxComponentLen else maxComponentLen
    if comp.length > maxLen then
      some s!"component too long ({comp.length} > {maxLen})"
    else
      none

/-- Check a full path for validity.
    Returns a list of error messages (empty if valid). -/
def checkPath (path : String) (portable : Bool) (strict : Bool := false) : List String :=
  if path.contains (λ c => c == '\x00') then
    ["null byte in path"]
  else
    let maxPLen := if portable then portableMaxPathLen else maxPathLen
    let errs :=
      if path.length > maxPLen then
        [s!"path too long ({path.length} > {maxPLen})"]
      else
        []
    let components := path.splitOn "/"
    let indexed := enumerate components
    let compErrors := List.filterMap (λ (i, comp) => checkComponent comp portable (i == 0) strict) indexed
    errs ++ compErrors

/-- Format errors into output lines. -/
def formatErrors (path : String) (errors : List String) : String :=
  String.intercalate "\n" (errors.map (λ e => s!"{path}: {e}"))

/-- Run a full path check, returning error messages. -/
def runCheck (path : String) (portable : Bool) (strict : Bool := false) : String :=
  formatErrors path (checkPath path portable strict)

/-- Check if a path is suitable for use as a file name. -/
def isValidPath (path : String) (portable : Bool) (strict : Bool := false) : Bool :=
  (checkPath path portable strict).isEmpty

-- ─── Proofs ──────────────────────────────────────────────────────────────────

example : checkComponent "" false false true = some "empty component" := by native_decide
example : checkComponent "" false false false = none := by native_decide
example : checkComponent "" false true = none := by native_decide
example : checkComponent "a" false false = none := by native_decide
example : checkComponent "a" true true = none := by native_decide
example : isPortableChar ' ' = false := rfl
example : isPortableChar '-' = true := rfl
example : isPortableChar '_' = true := rfl
example : isPortableChar '.' = true := rfl
example : checkPath "/usr/bin/ls" false = [] := by native_decide

end Lentils.Pathchk.Logic
