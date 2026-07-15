namespace Lentils.Csplit.Logic

structure Pattern where
  kind : Nat
  value : String
  offset : Int
  deriving Repr, BEq, DecidableEq

def Pattern.lineNo (n : Nat) : Pattern :=
  Pattern.mk 0 (toString n) 0

def Pattern.regex (re : String) (offset : Int) : Pattern :=
  Pattern.mk 1 re offset

structure Options where
  filePrefix : String := "xx"
  digits : Nat := 2
  quiet : Bool := false
  keepFiles : Bool := false
  elideEmpty : Bool := false
  suppressMatched : Bool := false
  deriving Repr

def parsePattern (s : String) : Option Pattern :=
  if s.toList.all (λ c => c >= '0' && c <= '9') then
    match s.toNat? with
    | some n => some (Pattern.lineNo n)
    | none => none
  else if s.startsWith "/" then
    let cs := s.toList.drop 1  -- remove leading /
    let rec takeUntil (acc : List Char) (remaining : List Char) : (List Char × List Char) :=
      match remaining with
      | [] => (acc.reverse, [])
      | '/' :: rest => (acc.reverse, rest)
      | c :: rest => takeUntil (c :: acc) rest
    let (reChars, after) := takeUntil [] cs
    let re := String.ofList reChars
    let offsetStr := String.ofList after
    let offset : Int :=
      if offsetStr.isEmpty then 0
      else
        -- Strip leading '+' if present (toInt? doesn't handle + prefix)
        let cleaned := if offsetStr.startsWith "+" then offsetStr.drop 1 else offsetStr
        match cleaned.toInt? with | some n => n | none => 0
    some (Pattern.regex re offset)
  else none

def parseArgs (args : List String) : Options × String × List Pattern :=
  let rec go (remaining : List String) (opts : Options) (patterns : List Pattern)
      : Options × String × List Pattern :=
    match remaining with
    | [] => (opts, "", patterns.reverse)
    | "-s" :: rest => go rest { opts with quiet := true } patterns
    | "--quiet" :: rest => go rest { opts with quiet := true } patterns
    | "-k" :: rest => go rest { opts with keepFiles := true } patterns
    | "--keep-files" :: rest => go rest { opts with keepFiles := true } patterns
    | "-z" :: rest => go rest { opts with elideEmpty := true } patterns
    | "--elide-empty-files" :: rest => go rest { opts with elideEmpty := true } patterns
    | "--suppress-matched" :: rest => go rest { opts with suppressMatched := true } patterns
    | "-f" :: p :: rest => go rest { opts with filePrefix := p } patterns
    | "--prefix" :: p :: rest => go rest { opts with filePrefix := p } patterns
    | "-n" :: d :: rest =>
      let digits := match d.toNat? with | some n => n | none => 2
      go rest { opts with digits := digits } patterns
    | "--digits" :: d :: rest =>
      let digits := match d.toNat? with | some n => n | none => 2
      go rest { opts with digits := digits } patterns
    | s :: rest =>
      if s.startsWith "-" && s != "-" then
        (opts, "", patterns.reverse)
      else
        let file := s
        let pats := rest.filterMap parsePattern
        (opts, file, pats)
  go args {} []

def formatStr (opts : Options) : String :=
  "%0" ++ toString opts.digits ++ "d"

def listExtract (xs : List String) (start stop : Nat) : List String :=
  xs.drop start |>.take (stop - start)

def computeSplits (lines : List String) (patterns : List Pattern) : List Nat :=
  let totalLines := lines.length
  let rec applyPatterns (remainingPatterns : List Pattern) (currentLine : Nat) (splits : List Nat) : List Nat :=
    match remainingPatterns with
    | [] => splits.reverse
    | pat :: rest =>
      if pat.kind = 0 then
        match pat.value.toNat? with
        | some n =>
          if n > currentLine + 1 && n <= totalLines then
            let splitPos := n - 1
            applyPatterns rest splitPos (splitPos :: splits)
          else
            applyPatterns rest currentLine splits
        | none => applyPatterns rest currentLine splits
      else
        let linesList := lines.drop currentLine
        let rec findIdx (i : Nat) (cs : List String) : Option Nat :=
          match cs with
          | [] => none
          | l :: ls => if l.contains pat.value then some i else findIdx (i + 1) ls
        let matchIdx := findIdx 0 linesList
        match matchIdx with
        | some idx =>
          let splitAt := currentLine + idx + 1 + pat.offset.toNat
          if splitAt > currentLine && splitAt <= totalLines then
            applyPatterns rest splitAt (splitAt :: splits)
          else
            applyPatterns rest currentLine splits
        | none => applyPatterns rest currentLine splits
  applyPatterns patterns 0 []

def splitLines (lines : List String) (splits : List Nat) : List (List String) :=
  let allSplits := 0 :: splits ++ [lines.length]
  let rec go (remainingSplits : List Nat) : List (List String) :=
    match remainingSplits with
    | [] => []
    | [start] => []
    | start :: stop :: rest =>
      let piece := listExtract lines start stop
      piece :: go (stop :: rest)
  go allSplits

theorem parse_prefix :
  (parseArgs ["-f", "chunk", "file", "5"]).1.filePrefix = "chunk" := by native_decide

theorem parse_digits :
  (parseArgs ["-n", "3", "file", "5"]).1.digits = 3 := by native_decide

theorem parse_pattern_line :
  parsePattern "5" = some (Pattern.lineNo 5) := by native_decide

/-- parsePattern: regex with no offset. -/
theorem parsePattern_regex :
  parsePattern "/foo/" = some (Pattern.regex "foo" 0) := by native_decide

/-- parsePattern: regex with positive offset. -/
theorem parsePattern_regex_plus :
  parsePattern "/foo/+1" = some (Pattern.regex "foo" 1) := by native_decide

/-- parsePattern: regex with negative offset. -/
theorem parsePattern_regex_minus :
  parsePattern "/foo/-1" = some (Pattern.regex "foo" (-1)) := by native_decide

/-- parsePattern: invalid returns none. -/
theorem parsePattern_invalid :
  parsePattern "" = none := by native_decide

/-- computeSplits: single line-number split. -/
theorem computeSplits_single :
  computeSplits ["a", "b", "c"] [Pattern.lineNo 2] = [1] := by native_decide

/-- computeSplits: multiple splits. -/
theorem computeSplits_multiple :
  computeSplits ["a", "b", "c", "d", "e"] [Pattern.lineNo 2, Pattern.lineNo 4] = [1, 3] := by native_decide

/-- computeSplits: split at end of file gives last index. -/
theorem computeSplits_end :
  computeSplits ["a", "b", "c"] [Pattern.lineNo 3] = [2] := by native_decide

/-- computeSplits: out-of-range line number is ignored. -/
theorem computeSplits_out_of_range :
  computeSplits ["a", "b"] [Pattern.lineNo 99] = [] := by native_decide

/-- splitLines: single split produces two pieces. -/
theorem splitLines_single :
  splitLines ["a", "b", "c"] [1] = [["a"], ["b", "c"]] := by native_decide

/-- splitLines: two splits produce three pieces. -/
theorem splitLines_two :
  splitLines ["a", "b", "c", "d"] [1, 3] = [["a"], ["b", "c"], ["d"]] := by native_decide

/-- splitLines: no splits produces one piece (entire file). -/
theorem splitLines_none :
  splitLines ["a", "b", "c"] [] = [["a", "b", "c"]] := by native_decide

end Lentils.Csplit.Logic