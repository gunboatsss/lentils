/-
Kconfig — Kconfig AST types and expression parser for Lentils.
0BSD

Pure-logic types for the Kconfig language. Expression parsing is done via
separate top-level functions to support `native_decide` proofs.

Provenance: linux/Documentation/kbuild/kconfig-language.rst
No GPL source was consulted.
-/

namespace Lentils.Kconfig

-- ─── AST Types ────────────────────────────────────────────────────────────────

inductive Expr
  | sym (name : String) | eq (l r : String) | ne (l r : String)
  | lt (l r : String) | gt (l r : String) | le (l r : String) | ge (l r : String)
  | not (e : Expr) | and (l r : Expr) | or (l r : Expr)
  deriving Repr, BEq, DecidableEq

inductive KType
  | bool | tristate | string | int | hex
  deriving Repr, BEq, DecidableEq

inductive Property
  | type (t : KType) (prompt : Option String)
  | defBool (e : Expr) | defTristate (e : Expr)
  | default (value : String) (guard : Option Expr)
  | dependsOn (e : Expr)
  | select (sym : String) (guard : Option Expr)
  | imply (sym : String) (guard : Option Expr)
  | range (low high : String) (guard : Option Expr)
  | modules | transitional | help (text : String)
  deriving Repr, BEq, DecidableEq

-- ─── Expression Tokenizer ─────────────────────────────────────────────────────

def exprTokens (s : String) : List String :=
  let chars := s.toList
  let rec go (remaining : List Char) (acc : List String) (current : List Char) : List String :=
    match remaining with
    | [] => (if current.isEmpty then acc else (String.ofList current.reverse) :: acc) |>.reverse
    | '(' :: rest =>
      let acc' := if current.isEmpty then acc else (String.ofList current.reverse) :: acc
      go rest ("(" :: acc') []
    | ')' :: rest =>
      let acc' := if current.isEmpty then acc else (String.ofList current.reverse) :: acc
      go rest (")" :: acc') []
    | ' ' :: rest => go rest (if current.isEmpty then acc else (String.ofList current.reverse) :: acc) []
    | '&' :: '&' :: rest =>
      let acc' := if current.isEmpty then acc else (String.ofList current.reverse) :: acc
      go rest ("&&" :: acc') []
    | '|' :: '|' :: rest =>
      let acc' := if current.isEmpty then acc else (String.ofList current.reverse) :: acc
      go rest ("||" :: acc') []
    | '!' :: '=' :: rest =>
      let acc' := if current.isEmpty then acc else (String.ofList current.reverse) :: acc
      go rest ("!=" :: acc') []
    | '!' :: rest =>
      let acc' := if current.isEmpty then acc else (String.ofList current.reverse) :: acc
      go rest ("!" :: acc') []
    | '=' :: rest =>
      let acc' := if current.isEmpty then acc else (String.ofList current.reverse) :: acc
      go rest ("=" :: acc') []
    | '<' :: '=' :: rest =>
      let acc' := if current.isEmpty then acc else (String.ofList current.reverse) :: acc
      go rest ("<=" :: acc') []
    | '>' :: '=' :: rest =>
      let acc' := if current.isEmpty then acc else (String.ofList current.reverse) :: acc
      go rest (">=" :: acc') []
    | '<' :: rest =>
      let acc' := if current.isEmpty then acc else (String.ofList current.reverse) :: acc
      go rest ("<" :: acc') []
    | '>' :: rest =>
      let acc' := if current.isEmpty then acc else (String.ofList current.reverse) :: acc
      go rest (">" :: acc') []
    | c :: rest => go rest acc (c :: current)
  go chars [] []

-- ─── Expression Helpers ───────────────────────────────────────────────────────

/-- Parse a single expression from a token list (single-pass). -/
-- Single-function recursive descent parser.
-- Handles prefix (!), primary (sym, compare, parens), then binop chain (&&, ||).
-- Each recursive call consumes at least one token, ensuring termination.
partial def parseExpr (ts : List String) : Option (Expr × List String) :=
  -- Parse prefix and primary
  let rec primary (ts' : List String) : Option (Expr × List String) :=
    match ts' with
    | "(" :: rest =>
      match parseExpr rest with
      | some (e, ")" :: rest') => some (e, rest')
      | _ => none
    | a :: "=" :: b :: rest => some (Expr.eq a b, rest)
    | a :: "!=" :: b :: rest => some (Expr.ne a b, rest)
    | a :: "<" :: b :: rest => some (Expr.lt a b, rest)
    | a :: ">" :: b :: rest => some (Expr.gt a b, rest)
    | a :: "<=" :: b :: rest => some (Expr.le a b, rest)
    | a :: ">=" :: b :: rest => some (Expr.ge a b, rest)
    | "!" :: rest =>
      match primary rest with
      | some (e, rest') => some (Expr.not e, rest')
      | _ => none
    | a :: rest => some (Expr.sym a, rest)
    | _ => none
  -- Get initial expression
  match primary ts with
  | none => none
  | some (l, rest) =>
    -- Handle binop chain (|| and &&)
    match rest with
    | "||" :: rest' =>
      match parseExpr rest' with
      | some (r, rest'') => some (Expr.or l r, rest'')
      | _ => none
    | "&&" :: rest' =>
      match parseExpr rest' with
      | some (r, rest'') => some (Expr.and l r, rest'')
      | _ => none
    | _ => some (l, rest)

/-- Parse a full expression string. -/
def parseExprString (s : String) : Option Expr :=
  match parseExpr (exprTokens s) with
  | some (e, []) => some e
  | _ => none

-- ─── Pretty Printer ───────────────────────────────────────────────────────────

def formatExpr (e : Expr) : String :=
  match e with
  | Expr.sym n => n | Expr.eq l r => l ++ "=" ++ r | Expr.ne l r => l ++ "!=" ++ r
  | Expr.lt l r => l ++ "<" ++ r | Expr.gt l r => l ++ ">" ++ r
  | Expr.le l r => l ++ "<=" ++ r | Expr.ge l r => l ++ ">=" ++ r
  | Expr.not e' => "!" ++ formatExpr e'
  | Expr.and l r => "(" ++ formatExpr l ++ " && " ++ formatExpr r ++ ")"
  | Expr.or l r => "(" ++ formatExpr l ++ " || " ++ formatExpr r ++ ")"

def formatType (t : KType) : String :=
  match t with | .bool => "bool" | .tristate => "tristate" | .string => "string" | .int => "int" | .hex => "hex"


-- ─── Kconfig File Validator ───────────────────────────────────────────────────

/-- Known Kconfig keywords for validation. -/
def knownKeywords : List String := [
  "config", "menuconfig", "bool", "tristate", "string", "int", "hex",
  "def_bool", "def_tristate", "prompt", "default", "depends", "on",
  "select", "imply", "range", "visible", "if", "endif",
  "menu", "endmenu", "choice", "endchoice", "comment", "source",
  "modules", "transitional", "help"
]

/-- A validation error. -/
structure KconfigError where
  line : Nat
  message : String
  deriving Repr

/-- Check if a line starts with a known keyword. Returns error or none. -/
def checkLine (lineNum : Nat) (line : String) : Option KconfigError :=
  let trimmed := String.trimAscii line |>.toString
  if trimmed.isEmpty || trimmed.startsWith "#" then
    none  -- skip empty/comment lines
  else
    -- Extract the first word (keyword)
    let firstWord := (trimmed.splitOn " ").head?.getD ""
    if knownKeywords.elem firstWord then
      none
    else if firstWord.startsWith "CONFIG_" then
      none  -- references to other configs are fine
    else
      some { line := lineNum + 1, message := "unknown keyword: '" ++ firstWord ++ "'" }

/-- Validate a complete Kconfig source. Returns list of errors (empty = valid). -/
def validate (src : String) : List KconfigError :=
  let lines := src.splitOn "\n"
  let numbered := List.range lines.length |>.zip lines
  numbered.filterMap (λ (i, l) => checkLine i l)

/-- Format validation errors for display. -/
def formatErrors (errors : List KconfigError) : String :=
  if errors.isEmpty then "Kconfig is valid."
  else
    String.intercalate "\n" (errors.map (λ e =>
      "line " ++ toString e.line ++ ": " ++ e.message))

-- ─── Proofs ──────────────────────────────────────────────────────────────────

theorem exprTokens_empty : exprTokens "" = [] := by native_decide

theorem exprTokens_sym : exprTokens "FOO" = ["FOO"] := by native_decide

theorem exprTokens_and : exprTokens "A && B" = ["A", "&&", "B"] := by native_decide

theorem formatType_bool : formatType .bool = "bool" := by native_decide

theorem formatType_tristate : formatType .tristate = "tristate" := by native_decide

theorem formatType_hex : formatType .hex = "hex" := by native_decide

end Lentils.Kconfig