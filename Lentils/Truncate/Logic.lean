/-
Truncate.Logic — Pure logic for the `truncate` utility.
0BSD

Contains only pure functions: argument parsing, size parsing.
No IO is performed here. All filesystem interaction lives in `truncate.lean`.
-/

namespace Lentils.Truncate.Logic

/--
Parsed options for `truncate`.
- `size` : `-s SIZE` / `--size=SIZE` — size to truncate to
- `reference` : `-r FILE` / `--reference=FILE` — use file's size
- `noCreate` : `-c` / `--no-create` — do not create the file
- `files` : the operands (files to truncate)
-/
structure Options where
  size : Option UInt64 := none
  reference : Option String := none
  noCreate : Bool := false
  files : List String := []
  deriving Repr

/--
Parse a size string (can include K, M, G suffixes).
Returns none on invalid format.
-/
def parseSize (s : String) : Option UInt64 :=
  if s.isEmpty then none else
    let chars := s.toList
    if chars.isEmpty then none else
      let lastChar := chars.getLast? |>.getD ' '
      if lastChar ∈ ['K', 'k', 'M', 'm', 'G', 'g'] then
        let numStr := String.ofList (chars.dropLast)
        match numStr.toNat? with
        | none => none
        | some n =>
          let multiplier : UInt64 :=
            match lastChar with
            | 'K' | 'k' => 1024
            | 'M' | 'm' => 1024 * 1024
            | 'G' | 'g' => 1024 * 1024 * 1024
            | _ => 1
          some (n.toUInt64 * multiplier)
      else
        match s.toNat? with
        | none => none
        | some n => some n.toUInt64

/--
Parse `truncate` arguments into `Options`.
-/
def parseArgs (args : List String) : Options :=
  let rec go (remaining : List String) (opts : Options) : Options :=
    match remaining with
    | [] => opts
    | "--" :: rest => { opts with files := opts.files ++ rest }
    | "-c" :: rest => go rest { opts with noCreate := true }
    | "--no-create" :: rest => go rest { opts with noCreate := true }
    | "-s" :: s :: rest => go rest { opts with size := parseSize s }
    | "--size" :: s :: rest => go rest { opts with size := parseSize s }
    | "-r" :: s :: rest => go rest { opts with reference := some s }
    | "--reference" :: s :: rest => go rest { opts with reference := some s }
    | s :: rest =>
      if s.startsWith "-" && s.length > 1 then
        { opts with files := opts.files ++ s :: rest }
      else
        go rest { opts with files := opts.files ++ [s] }
  go args {}

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- Parsing `-c` sets noCreate. -/
example : (parseArgs ["-c", "file"]).noCreate = true := by native_decide

/-- Parsing a plain operand becomes a file. -/
example : (parseArgs ["file"]).files = ["file"] := by native_decide

/-- Parse size with K suffix. -/
example : parseSize "1K" = some 1024 := by native_decide

/-- Parse size with M suffix. -/
example : parseSize "1M" = some (1024*1024) := by native_decide
