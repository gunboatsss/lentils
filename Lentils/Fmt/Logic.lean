/-
Fmt.Logic — Verified pure logic for `fmt`. 0BSD
-/

namespace Lentils.Fmt.Logic

structure FmtInput where
  input : String := ""
  width : Nat := 75
  deriving Inhabited

abbrev FmtOutput := String

def isBlank (l : String) : Bool := l.trimAscii.isEmpty

def splitParagraphs (input : String) : List (List String) :=
  let lines := input.splitOn "\n"
  let rec go (ls : List String) (cur : List String) (acc : List (List String)) : List (List String) :=
    match ls with
    | [] => if cur.isEmpty then acc.reverse else (cur.reverse :: acc).reverse
    | l :: rest => if isBlank l then if cur.isEmpty then go rest [] acc else go rest [] (cur.reverse :: acc) else go rest (l :: cur) acc
  go lines [] []

def joinParagraph (lines : List String) : String :=
  match lines with
  | [] => ""
  | first :: rest => let rec go (acc : String) (ls : List String) : String := match ls with | [] => acc | l :: ls => go (acc ++ " " ++ l) ls; go first rest

def breakWord (w : String) (width : Nat) : List String :=
  if width = 0 then [w]
  else if w.isEmpty then []
  else
    let cs := w.toList; let nChunks := (cs.length + width - 1) / width
    List.range nChunks |>.map (λ i => String.ofList ((cs.drop (i * width)).take width))

def wrapLine (text : String) (width : Nat) : List String :=
  if width = 0 then [text]
  else
    let words := text.splitOn " " |>.filter (not ·.isEmpty)
    let rec go (ws : List String) (cur : String) (acc : List String) : List String :=
      match ws with
      | [] => (cur :: acc).reverse
      | w :: rest =>
        let cand := if cur.isEmpty then w else s!"{cur} {w}"
        if cand.length ≤ width then go rest cand acc
        else
          let acc1 := cur :: acc; let pieces := breakWord w width; let pl := pieces.length
          let init := pieces.take (pl - 1); let last := (pieces.drop (pl - 1)).headD ""
          go rest last (init.reverse ++ acc1)
    (go words "" []).filter (not ·.isEmpty)

def fmt (input : String) (width : Nat := 75) : String :=
  let paras := splitParagraphs input
  let wrapped := paras.map (λ p => String.intercalate "\n" (wrapLine (joinParagraph p) width))
  String.intercalate "\n\n" wrapped

def spec (input : FmtInput) : FmtOutput :=
  fmt input.input input.width

/--
I2: Empty input → empty output.
-/
theorem i_empty : fmt "" 75 = "" := by
  native_decide

/--
I3: isBlank on empty string returns true.
-/
theorem i_isBlank_empty : isBlank "" = true := by
  native_decide

/--
I4: "hello world" at large width is unchanged.
-/
theorem i_example_unchanged : fmt "hello world" 80 = "hello world" := by
  native_decide

/--
I5: Zero width is a fixed point for `wrapLine` (parametric, proved by unfold + simp).
-/
theorem i_wrapLine_zero (text : String) : wrapLine text 0 = [text] := by
  unfold wrapLine
  simp

example : fmt "" 75 = "" := i_empty
example : fmt "hello world" 80 = "hello world" := i_example_unchanged
example : fmt "hello world" 5 = "hello\nworld" := by native_decide
example : fmt "a\n\nb" 80 = "a\n\nb" := by native_decide

end Lentils.Fmt.Logic
