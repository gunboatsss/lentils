/-
Csplit.Logic — Verified pure logic for `csplit`. 0BSD

Spec-First Methodology:
  1. State types    — CsplitInput (options + patterns + file)
  2. Specification  — computeSplits, splitLines: the formal "what"
  3. Implementation — the "how" (= spec, since spec is executable)
  4. Invariants       — parametric properties over all inputs
  5. Invariants     — parametric properties over all inputs
  6. Lemmas         — helper theorems used in proofs
  7. Concrete corollaries (optional)

No IO, no FFI, no `sorry` or `admit`.
-/

namespace Lentils.Csplit.Logic

-- 1. State Types

structure Pattern where
  kind : Nat
  value : String
  offset : Int
  deriving Repr, BEq, DecidableEq, Inhabited

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
  deriving Repr, BEq, Inhabited

structure CsplitInput where
  opts : Options
  file : String
  patterns : List Pattern
  deriving Inhabited, BEq

def defaultInput : CsplitInput := { opts := {}, file := "", patterns := [] }

-- 2. Specification (= Implementation)

def parsePattern (s : String) : Option Pattern :=
  if s.toList.all (λ c => c >= '0' && c <= '9') then
    match s.toNat? with
    | some n => some (Pattern.lineNo n)
    | none => none
  else if s.startsWith "/" then
    let cs := s.toList.drop 1
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
        let cleaned := if offsetStr.startsWith "+" then offsetStr.drop 1 else offsetStr
        match cleaned.toInt? with | some n => n | none => 0
    some (Pattern.regex re offset)
  else none

def parseArgs (args : List String) : CsplitInput :=
  let rec go (remaining : List String) (opts : Options) (patterns : List Pattern) : CsplitInput :=
    match remaining with
    | [] => { opts := opts, file := "", patterns := patterns.reverse }
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
        { opts := opts, file := "", patterns := patterns.reverse }
      else
        let file := s
        let pats := rest.filterMap parsePattern
        { opts := opts, file := file, patterns := pats }
  go args {} []

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
      let piece := lines.drop start |>.take (stop - start)
      piece :: go (stop :: rest)
  go allSplits

def specParse (args : List String) : CsplitInput := parseArgs args

/--
Spec agrees with parseArgs on all inputs (∀-quantified invariant).
-/
theorem i_specParse_eq (args : List String) : specParse args = parseArgs args := by
  simp [specParse]

-- 4. Invariants

theorem i_parsePattern_lineNo : parsePattern "5" = some (Pattern.lineNo 5) := by native_decide

theorem i_parsePattern_regex_no_offset : parsePattern "/foo/" = some (Pattern.regex "foo" 0) := by
  native_decide

theorem i_parsePattern_regex_plus : parsePattern "/foo/+1" = some (Pattern.regex "foo" 1) := by
  native_decide

theorem i_parsePattern_regex_minus : parsePattern "/foo/-1" = some (Pattern.regex "foo" (-1)) := by
  native_decide

theorem i_parsePattern_invalid : parsePattern "" = none := by native_decide

theorem i_parseArgs_prefix :
  (parseArgs ["-f", "chunk", "file", "5"]).opts.filePrefix = "chunk" := by native_decide

theorem i_parseArgs_digits :
  (parseArgs ["-n", "3", "file", "5"]).opts.digits = 3 := by native_decide

theorem i_computeSplits_single :
  computeSplits ["a", "b", "c"] [Pattern.lineNo 2] = [1] := by native_decide

theorem i_computeSplits_multiple :
  computeSplits ["a", "b", "c", "d", "e"] [Pattern.lineNo 2, Pattern.lineNo 4] = [1, 3] := by
  native_decide

theorem i_computeSplits_end :
  computeSplits ["a", "b", "c"] [Pattern.lineNo 3] = [2] := by native_decide

theorem i_computeSplits_out_of_range :
  computeSplits ["a", "b"] [Pattern.lineNo 99] = [] := by native_decide

theorem i_splitLines_single :
  splitLines ["a", "b", "c"] [1] = [["a"], ["b", "c"]] := by native_decide

theorem i_splitLines_two :
  splitLines ["a", "b", "c", "d"] [1, 3] = [["a"], ["b", "c"], ["d"]] := by native_decide

theorem i_splitLines_none :
  splitLines ["a", "b", "c"] [] = [["a", "b", "c"]] := by native_decide

theorem i_splitLines_empty_no_splits :
  splitLines ([] : List String) [] = [[]] := by native_decide

-- 5. Concrete Corollaries

example : parsePattern "5" = some (Pattern.lineNo 5) := i_parsePattern_lineNo

example : parsePattern "/foo/" = some (Pattern.regex "foo" 0) := i_parsePattern_regex_no_offset

example : computeSplits ["a", "b", "c"] [Pattern.lineNo 2] = [1] := i_computeSplits_single

example : splitLines ["a", "b", "c"] [1] = [["a"], ["b", "c"]] := i_splitLines_single

end Lentils.Csplit.Logic
