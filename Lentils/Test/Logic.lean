/-
Test.Logic — Pure expression evaluation for `test` / `[`. 0BSD
-/

namespace Lentils.Test.Logic

def exitTrue : UInt32 := 0
def exitFalse : UInt32 := 1

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

def eval (e : Expr) : Bool :=
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
  | Expr.notExpr e' => ¬ eval e'
  | Expr.andExpr e1 e2 => eval e1 && eval e2
  | Expr.orExpr e1 e2 => eval e1 || eval e2
  | Expr.trueExpr => true
  | Expr.falseExpr => false

def parseExpr (args : List String) : Option (List String × Expr) :=
  match args with
  | [] => some ([], Expr.falseExpr)
  | "!" :: rest =>
    match parseExpr rest with
    | some (remaining, e) => some (remaining, Expr.notExpr e)
    | none => none
  | "-n" :: s :: rest => some (rest, Expr.nTest s)
  | "-z" :: s :: rest => some (rest, Expr.zTest s)
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
  | some e => boolToExit (eval e)
  | none => exitFalse

-- Theorems (avoiding partial functions)

theorem exitTrue_zero : exitTrue = 0 := rfl
theorem exitFalse_one : exitFalse = 1 := rfl

theorem eval_stringLit_empty : eval (Expr.stringLit "") = false := rfl
theorem eval_stringLit_nonempty : eval (Expr.stringLit "hello") = true := rfl

theorem eval_zTest_empty : eval (Expr.zTest "") = true := rfl
theorem eval_zTest_nonempty : eval (Expr.zTest "hello") = false := rfl

theorem eval_nTest_empty : eval (Expr.nTest "") = false := rfl
theorem eval_nTest_nonempty : eval (Expr.nTest "hello") = true := rfl

theorem eval_eqTest_true : eval (Expr.eqTest "abc" "abc") = true := rfl
theorem eval_eqTest_false : eval (Expr.eqTest "abc" "def") = false := rfl

theorem eval_neqTest_true : eval (Expr.neqTest "abc" "def") = true := rfl
theorem eval_neqTest_false : eval (Expr.neqTest "abc" "abc") = false := rfl

theorem eval_intEq_true : eval (Expr.intEq "42" "42") = true := by native_decide
theorem eval_intEq_false : eval (Expr.intEq "42" "0") = false := by native_decide

theorem eval_intLt_true : eval (Expr.intLt "5" "10") = true := by native_decide
theorem eval_intLt_false : eval (Expr.intLt "10" "5") = false := by native_decide

theorem eval_not_true : eval (Expr.notExpr Expr.falseExpr) = true := rfl
theorem eval_not_false : eval (Expr.notExpr Expr.trueExpr) = false := rfl

theorem eval_and_both : eval (Expr.andExpr Expr.trueExpr Expr.trueExpr) = true := rfl
theorem eval_and_one : eval (Expr.andExpr Expr.trueExpr Expr.falseExpr) = false := rfl

theorem eval_or_both : eval (Expr.orExpr Expr.trueExpr Expr.falseExpr) = true := rfl
theorem eval_or_neither : eval (Expr.orExpr Expr.falseExpr Expr.falseExpr) = false := rfl

theorem boolToExit_true : boolToExit true = 0 := rfl
theorem boolToExit_false : boolToExit false = 1 := rfl

theorem parseInt_zero : parseInt "0" = some (0 : Int) := by native_decide
theorem parseInt_positive : parseInt "42" = some (42 : Int) := by native_decide
theorem parseInt_negative : parseInt "-42" = some (-42 : Int) := by native_decide
theorem parseInt_empty : parseInt "" = none := rfl

end Lentils.Test.Logic
