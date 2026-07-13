/-
Paste.Logic — Pure line-merging logic for `paste`. 0BSD -/
namespace Lentils.Paste.Logic

/--
Paste lines from multiple lists together, separated by delimiter.
Each list represents lines from one file.
With no files (empty list of lists), returns empty string.
-/
def paste (fileLines : List (List String)) (delim : String := "\t") : String :=
  match fileLines with
  | [] => ""
  | [single] => String.intercalate "\n" single
  | multiple =>
    -- Find the maximum number of lines across all files
    let maxLines := multiple.foldl (λ m lines => max m lines.length) 0
    -- Build output line by line
    let lines := List.range maxLines |>.map (λ i =>
      let parts := multiple.map (λ lines =>
        match lines.drop i with
        | x :: _ => x
        | [] => "")
      String.intercalate delim parts)
    String.intercalate "\n" lines

/--
Paste in serial mode (-s): merge all lines from each file into a single line.
-/
def pasteSerial (fileLines : List (List String)) (delim : String := "\t") : String :=
  let lines := fileLines.map (λ lines => String.intercalate delim lines)
  String.intercalate "\n" lines

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- paste of empty inputs yields empty. -/
example : paste [] "	" = "" := by
  native_decide

/-- paste of one file with one line. -/
example : paste [["a"]] "," = "a" := by
  native_decide

end Lentils.Paste.Logic


