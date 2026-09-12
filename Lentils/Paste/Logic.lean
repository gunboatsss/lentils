/-
Paste.Logic — Verified pure logic for `paste`. 0BSD
-/

namespace Lentils.Paste.Logic

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

structure PasteInput where
  fileLines : List (List String) := []
  delim : String := "\t"
  serial : Bool := false
  deriving Inhabited

abbrev PasteOutput := String

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Core functions
-- ═══════════════════════════════════════════════════════════════════════════════════

def paste (fileLines : List (List String)) (delim : String := "\t") : String :=
  match fileLines with
  | [] => ""
  | [single] => String.intercalate "\n" single
  | multiple =>
    let maxLines := multiple.foldl (λ m lines => max m lines.length) 0
    let lines := List.range maxLines |>.map (λ i =>
      let parts := multiple.map (λ lines => match lines.drop i with | x :: _ => x | [] => "")
      String.intercalate delim parts)
    String.intercalate "\n" lines

def pasteSerial (fileLines : List (List String)) (delim : String := "\t") : String :=
  let lines := fileLines.map (λ lines => String.intercalate delim lines)
  String.intercalate "\n" lines

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Specification, Implementation, Correctness
-- ═══════════════════════════════════════════════════════════════════════════════════

def spec (input : PasteInput) : PasteOutput :=
  if input.serial then pasteSerial input.fileLines input.delim
  else paste input.fileLines input.delim

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Parametric Invariants
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: paste with no files yields empty (∀ delim).
-/
theorem i_empty (delim : String) : paste [] delim = "" := by
  simp [paste]

/--
I2: paste of one file yields file contents unchanged (∀ lines, delim).
-/
theorem i_single_file (lines : List String) (delim : String) : paste [lines] delim = String.intercalate "\n" lines := by
  simp [paste]

/--
I3: Serial paste of one file yields that file's lines joined by delim (∀ lines, delim).
-/
theorem i_serial_single (lines : List String) (delim : String) : pasteSerial [lines] delim = String.intercalate delim lines := rfl

/--
I4: Serial `spec` with the serial flag set delegates to `pasteSerial` (∀ input).
-/
theorem i_spec_serial (input : PasteInput) (h : input.serial = true) :
    spec input = pasteSerial input.fileLines input.delim := by
  simp [spec, h]

/--
I5: Serial paste with no files yields empty (∀ delim).
-/
theorem i_serial_empty (delim : String) : pasteSerial [] delim = "" := rfl

/--
I6: String.intercalate of empty list is empty (∀ s).
-/
theorem i_intercalate_nil (s : String) : String.intercalate s [] = "" := rfl

/--
I7: String.intercalate of a single element returns the element (∀ s, x).
-/
theorem i_intercalate_single (s : String) (x : String) : String.intercalate s [x] = x := rfl

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

example : paste [] "\t" = "" := i_empty "\t"
example : paste [["a"]] "," = "a" := by native_decide
example : paste [["a", "b"], ["1", "2"]] "," = "a,1\nb,2" := by native_decide
example : pasteSerial [["a", "b"], ["1", "2"]] "," = "a,b\n1,2" := by native_decide

end Lentils.Paste.Logic
