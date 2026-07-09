/-
Test.Logic — Pure expression evaluation for `test` / `[`. 0BSD
-/

namespace Lentils.Test.Logic

def exitTrue : UInt32 := 0
def exitFalse : UInt32 := 1

-- StatContext bundles the result of a `stat`/`access` call for one path.
-- The IO layer fills this in; the pure evaluator reads it.
structure StatContext where
  pathExists : Bool
  isFile : Bool
  isDir : Bool
  size : UInt64
  readable : Bool
  writable : Bool
  executable : Bool
  deriving Inhabited

-- Default context used when no real stat is available (e.g. in proofs or
-- pure runs).  Every predicate returns `false`.
def defaultCtx : StatContext :=
  { pathExists := false
  , isFile := false
  , isDir := false
  , size := 0
  , readable := false
  , writable := false
  , executable := false
  }

inductive Expr
  | stringLit : String → Expr
  | nTest : String → Expr
  | zTest : String → Expr
  | eqTest : String → String → Expr
  | neqTest : String → String → Expr
  | intEq : String → String → Expr
  | intNe : String → String → Expr
  | intLt : String → String → Expr
  | intLe : String → String → Expr
  | intGt : String → String → Expr
  | intGe : String → String → Expr
  | notExpr : Expr → Expr
  | andExpr : Expr → Expr → Expr
  | orExpr : Expr → Expr → Expr
  | trueExpr : Expr
  | falseExpr : Expr
  -- file-test operators (POSIX)
  | fileIsFile (path : String) : Expr
  | fileIsDir (path : String) : Expr
  | fileExists (path : String) : Expr
  | fileNotEmpty (path : String) : Expr
  | fileReadable (path : String) : Expr
  | fileWritable (path : String) : Expr
  | fileExecutable (path : String) : Expr
  deriving Inhabited

def parseInt (s : String) : Option Int :=
  if s.isEmpty then none
  else if s == "-" then none
  else if s.startsWith "-" then
    match (s.drop 1).toString.toNat? with
    | some n => some (-(Int.ofNat n))
    | none => none
  else
    match s.toNat? with
    | some n => some (Int.ofNat n)
    | none => none

def eval (lookup : String → StatContext) (e : Expr) : Bool :=
  match e with
  | Expr.stringLit s => ¬ s.isEmpty
  | Expr.nTest s => ¬ s.isEmpty
  | Expr.zTest s => s.isEmpty
  | Expr.eqTest s1 s2 => s1 = s2
  | Expr.neqTest s1 s2 => s1 ≠ s2
  | Expr.intEq s1 s2 =>
    match parseInt s1, parseInt s2 with
    | some n1, some n2 => n1 = n2
    | _, _ => false
  | Expr.intNe s1 s2 =>
    match parseInt s1, parseInt s2 with
    | some n1, some n2 => n1 ≠ n2
    | _, _ => false
  | Expr.intLt s1 s2 =>
    match parseInt s1, parseInt s2 with
    | some n1, some n2 => n1 < n2
    | _, _ => false
  | Expr.intLe s1 s2 =>
    match parseInt s1, parseInt s2 with
    | some n1, some n2 => n1 ≤ n2
    | _, _ => false
  | Expr.intGt s1 s2 =>
    match parseInt s1, parseInt s2 with
    | some n1, some n2 => n1 > n2
    | _, _ => false
  | Expr.intGe s1 s2 =>
    match parseInt s1, parseInt s2 with
    | some n1, some n2 => n1 ≥ n2
    | _, _ => false
  | Expr.notExpr e' => ¬ eval lookup e'
  | Expr.andExpr e1 e2 => eval lookup e1 && eval lookup e2
  | Expr.orExpr e1 e2 => eval lookup e1 || eval lookup e2
  | Expr.trueExpr => true
  | Expr.falseExpr => false
  -- file-test operators: consult the StatContext via lookup
  | Expr.fileIsFile path =>
    let ctx := lookup path; ctx.pathExists && ctx.isFile
  | Expr.fileIsDir path =>
    let ctx := lookup path; ctx.pathExists && ctx.isDir
  | Expr.fileExists path =>
    (lookup path).pathExists
  | Expr.fileNotEmpty path =>
    let ctx := lookup path; ctx.pathExists && ctx.size > 0
  | Expr.fileReadable path =>
    (lookup path).readable
  | Expr.fileWritable path =>
    (lookup path).writable
  | Expr.fileExecutable path =>
    (lookup path).executable

def parseExpr (args : List String) : Option (List String × Expr) :=
  match args with
  | [] => some ([], Expr.falseExpr)
  | "!" :: rest =>
    match parseExpr rest with
    | some (remaining, e) => some (remaining, Expr.notExpr e)
    | none => none
  | "-n" :: s :: rest => some (rest, Expr.nTest s)
  | "-z" :: s :: rest => some (rest, Expr.zTest s)
  -- file-test operators
  | "-f" :: path :: rest => some (rest, Expr.fileIsFile path)
  | "-d" :: path :: rest => some (rest, Expr.fileIsDir path)
  | "-e" :: path :: rest => some (rest, Expr.fileExists path)
  | "-s" :: path :: rest => some (rest, Expr.fileNotEmpty path)
  | "-r" :: path :: rest => some (rest, Expr.fileReadable path)
  | "-w" :: path :: rest => some (rest, Expr.fileWritable path)
  | "-x" :: path :: rest => some (rest, Expr.fileExecutable path)
  | s1 :: "=" :: s2 :: rest => some (rest, Expr.eqTest s1 s2)
  | s1 :: "!=" :: s2 :: rest => some (rest, Expr.neqTest s1 s2)
  | s1 :: "-eq" :: s2 :: rest => some (rest, Expr.intEq s1 s2)
  | s1 :: "-ne" :: s2 :: rest => some (rest, Expr.intNe s1 s2)
  | s1 :: "-lt" :: s2 :: rest => some (rest, Expr.intLt s1 s2)
  | s1 :: "-le" :: s2 :: rest => some (rest, Expr.intLe s1 s2)
  | s1 :: "-gt" :: s2 :: rest => some (rest, Expr.intGt s1 s2)
  | s1 :: "-ge" :: s2 :: rest => some (rest, Expr.intGe s1 s2)
  | [s] => some ([], Expr.stringLit s)
  | _ => none

partial def parseArgs (args : List String) : Option Expr :=
  match parseExpr args with
  | none => none
  | some ([], e) => some e
  | some ("-a" :: rest, e1) =>
    match parseArgs rest with
    | some e2 => some (Expr.andExpr e1 e2)
    | none => none
  | some ("-o" :: rest, e1) =>
    match parseArgs rest with
    | some e2 => some (Expr.orExpr e1 e2)
    | none => none
  | some (_, e1) => some e1

def boolToExit (b : Bool) : UInt32 := if b then exitTrue else exitFalse

def runPure (args : List String) : UInt32 :=
  match parseArgs args with
  | some e => boolToExit (eval (λ _ => defaultCtx) e)
  | none => exitFalse

-- Theorems (avoiding partial functions)
-- All non-file-expression theorems use `(λ _ => defaultCtx)` and `native_decide`.

theorem exitTrue_zero : exitTrue = 0 := rfl
theorem exitFalse_one : exitFalse = 1 := rfl

def defLookup : String → StatContext := λ _ => defaultCtx

theorem eval_stringLit_empty : eval defLookup (Expr.stringLit "") = false := rfl
theorem eval_stringLit_nonempty : eval defLookup (Expr.stringLit "hello") = true := rfl

theorem eval_zTest_empty : eval defLookup (Expr.zTest "") = true := rfl
theorem eval_zTest_nonempty : eval defLookup (Expr.zTest "hello") = false := rfl

theorem eval_nTest_empty : eval defLookup (Expr.nTest "") = false := rfl
theorem eval_nTest_nonempty : eval defLookup (Expr.nTest "hello") = true := rfl

theorem eval_eqTest_true : eval defLookup (Expr.eqTest "abc" "abc") = true := rfl
theorem eval_eqTest_false : eval defLookup (Expr.eqTest "abc" "def") = false := rfl

theorem eval_neqTest_true : eval defLookup (Expr.neqTest "abc" "def") = true := rfl
theorem eval_neqTest_false : eval defLookup (Expr.neqTest "abc" "abc") = false := rfl

theorem eval_intEq_true : eval defLookup (Expr.intEq "42" "42") = true := by native_decide
theorem eval_intEq_false : eval defLookup (Expr.intEq "42" "0") = false := by native_decide

theorem eval_intLt_true : eval defLookup (Expr.intLt "5" "10") = true := by native_decide
theorem eval_intLt_false : eval defLookup (Expr.intLt "10" "5") = false := by native_decide

theorem eval_not_true : eval defLookup (Expr.notExpr Expr.falseExpr) = true := rfl
theorem eval_not_false : eval defLookup (Expr.notExpr Expr.trueExpr) = false := rfl

theorem eval_and_both : eval defLookup (Expr.andExpr Expr.trueExpr Expr.trueExpr) = true := rfl
theorem eval_and_one : eval defLookup (Expr.andExpr Expr.trueExpr Expr.falseExpr) = false := rfl

theorem eval_or_both : eval defLookup (Expr.orExpr Expr.trueExpr Expr.falseExpr) = true := rfl
theorem eval_or_neither : eval defLookup (Expr.orExpr Expr.falseExpr Expr.falseExpr) = false := rfl

theorem boolToExit_true : boolToExit true = 0 := rfl
theorem boolToExit_false : boolToExit false = 1 := rfl

theorem parseInt_zero : parseInt "0" = some (0 : Int) := by native_decide
theorem parseInt_positive : parseInt "42" = some (42 : Int) := by native_decide
theorem parseInt_negative : parseInt "-42" = some (-42 : Int) := by native_decide
theorem parseInt_empty : parseInt "" = none := rfl

end Lentils.Test.Logic
