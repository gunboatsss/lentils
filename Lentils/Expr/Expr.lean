/-
Expr — IO wrapper for the `expr` utility. 0BSD

Evaluates integer/string expressions.
Uses Expr.Logic (pure, verified) for parsing and evaluation.
-/

import Lentils.Expr.Logic

namespace Lentils.Expr

open Logic

/-- The expr utility: concatenates arguments and evaluates as an expression. -/
def run (args : List String) : IO UInt32 := do
  if args.isEmpty then
    IO.eprintln "expr: missing operand"
    return 1
  let input := String.intercalate " " args
  match evaluate input with
  | some v =>
    let output := formatValue v
    IO.println output
    if v == Value.bool false || v == Value.int 0 || (match v with | Value.str "" => true | _ => false) then
      return 1
    else
      return 0
  | none =>
    IO.eprintln "expr: syntax error"
    return 2

end Lentils.Expr
