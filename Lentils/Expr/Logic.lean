/-
Expr.Logic — Pure expression evaluator for `expr`. Recursive descent parser.
-/

namespace Lentils.Expr.Logic

inductive Token where
  | int (v : Int)
  | str (s : String)
  | plus | minus | star | slash | perc
  | eq | ne | lt | le | gt | ge
  | colon | pipe | amp
  | lparen | rparen
  | eof
  deriving Repr, BEq, DecidableEq

inductive Value where
  | int (i : Int)
  | str (s : String)
  | bool (b : Bool)
  deriving Repr, BEq, DecidableEq

/-- List indexing helper. -/
def listGet (cs : List Char) (i : Nat) : Char :=
  match cs, i with
  | [], _ => '?'
  | c :: _, 0 => c
  | _ :: rest, n+1 => listGet rest n

/-- Tokenize input using toList for char access. -/
partial def tokenize (s : String) : List Token :=
  let chars := s.toList
  let rec go (pos : Nat) (acc : List Token) : List Token :=
    if pos ≥ chars.length then
      (Token.eof :: acc).reverse
    else
      let c := listGet chars pos
      if c == ' ' || c == '\t' then
        go (pos + 1) acc
      else if c.isDigit then
        let rec numEnd (i : Nat) : Nat :=
          if i < chars.length && (listGet chars i).isDigit then numEnd (i + 1) else i
        let endPos := numEnd pos
        let numStr := String.ofList (chars.extract pos endPos)
        match numStr.toInt? with
        | some n => go endPos (Token.int n :: acc)
        | none => go endPos (Token.str numStr :: acc)
      else
        let token : Token :=
          match c with
          | '+' => Token.plus
          | '-' => Token.minus
          | '*' => Token.star
          | '/' => Token.slash
          | '%' => Token.perc
          | '=' => Token.eq
          | '!' =>
            if pos + 1 < chars.length && (listGet chars (pos + 1)) == '=' then
              Token.ne
            else Token.str "!"
          | '<' =>
            if pos + 1 < chars.length && (listGet chars (pos + 1)) == '=' then
              Token.le
            else Token.lt
          | '>' =>
            if pos + 1 < chars.length && (listGet chars (pos + 1)) == '=' then
              Token.ge
            else Token.gt
          | ':' => Token.colon
          | '|' => Token.pipe
          | '&' => Token.amp
          | '(' => Token.lparen
          | ')' => Token.rparen
          | _ => Token.str (String.singleton c)
        let skip : Nat :=
          match token with
          | Token.ne | Token.le | Token.ge => 2
          | _ => 1
        go (pos + skip) (token :: acc)
  go 0 []

def isTrue (v : Value) : Bool :=
  match v with
  | Value.int i => i != 0
  | Value.str s => !s.isEmpty
  | Value.bool b => b

def equal (a b : Value) : Bool :=
  match a, b with
  | Value.int x, Value.int y => x == y
  | Value.str x, Value.str y => x == y
  | Value.bool x, Value.bool y => x == y
  | Value.int x, Value.str y => toString x == y
  | Value.str x, Value.int y => x == toString y
  | _, _ => false

partial def compare (a b : Value) : Ordering :=
  match a, b with
  | Value.int x, Value.int y => if x < y then Ordering.lt else if x > y then Ordering.gt else Ordering.eq
  | Value.str x, Value.str y => if x < y then Ordering.lt else if x > y then Ordering.gt else Ordering.eq
  | Value.int x, Value.str y => compare (Value.str (toString x)) (Value.str y)
  | Value.str x, Value.int y => compare (Value.str x) (Value.str (toString y))
  | _, _ => Ordering.eq

def add (a b : Value) : Value :=
  match a, b with
  | Value.int x, Value.int y => Value.int (x + y)
  | Value.str x, Value.str y => Value.str (x ++ y)
  | Value.int x, Value.str y => Value.str (toString x ++ y)
  | Value.str x, Value.int y => Value.str (x ++ toString y)
  | _, _ => Value.int 0

def sub (a b : Value) : Value :=
  match a, b with
  | Value.int x, Value.int y => Value.int (x - y)
  | _, _ => Value.int 0

def mul (a b : Value) : Value :=
  match a, b with
  | Value.int x, Value.int y => Value.int (x * y)
  | _, _ => Value.int 0

def div (a b : Value) : Value :=
  match a, b with
  | Value.int x, Value.int y => if y == 0 then Value.int 0 else Value.int (x / y)
  | _, _ => Value.int 0

def mod (a b : Value) : Value :=
  match a, b with
  | Value.int x, Value.int y => if y == 0 then Value.int 0 else Value.int (x % y)
  | _, _ => Value.int 0

def neg (a : Value) : Value :=
  match a with
  | Value.int x => Value.int (-x)
  | _ => a

-- Parser (mutual recursion)
mutual
  /-- Handle implicit string concatenation: adjacent primitives are concatenated.
      This is called after parsing a primary, to consume following primaries. -/
  partial def parseConcat (val : Value) (tokens : List Token) : Option (Value × List Token) :=
    match tokens with
    | [] => some (val, [])
    | (Token.int n) :: rest =>
      match val with
      | Value.str s => parseConcat (Value.str (s ++ toString n)) rest
      | _ => parseConcat (Value.int n) rest
    | (Token.str s) :: rest =>
      match val with
      | Value.str s' => parseConcat (Value.str (s' ++ s)) rest
      | Value.int n => parseConcat (Value.str (toString n ++ s)) rest
      | Value.bool _ => parseConcat (Value.str s) rest
    | _ => some (val, tokens)

  partial def parsePrimary (tokens : List Token) : Option (Value × List Token) :=
    match tokens with
    | Token.int v :: rest => parseConcat (Value.int v) rest
    | Token.str s :: rest => parseConcat (Value.str s) rest
    | Token.minus :: rest =>
      match parsePrimary rest with
      | some (v, rest') => parseConcat (neg v) rest'
      | none => none
    | Token.lparen :: rest =>
      match parseExpr rest with
      | some (v, Token.rparen :: rest'') => parseConcat v rest''
      | _ => none
    | _ => none

  partial def parseTerm (tokens : List Token) : Option (Value × List Token) :=
    match parsePrimary tokens with
    | some (left, Token.star :: rest) =>
      match parseTerm rest with
      | some (right, rest') => some (mul left right, rest')
      | none => some (left, rest)
    | some (left, Token.slash :: rest) =>
      match parseTerm rest with
      | some (right, rest') => some (div left right, rest')
      | none => some (left, rest)
    | some (left, Token.perc :: rest) =>
      match parseTerm rest with
      | some (right, rest') => some (mod left right, rest')
      | none => some (left, rest)
    | r => r

  partial def parseArith (tokens : List Token) : Option (Value × List Token) :=
    match parseTerm tokens with
    | some (left, Token.plus :: rest) =>
      match parseArith rest with
      | some (right, rest') => some (add left right, rest')
      | none => some (left, rest)
    | some (left, Token.minus :: rest) =>
      match parseArith rest with
      | some (right, rest') => some (sub left right, rest')
      | none => some (left, rest)
    | r => r

  partial def parseCompare (tokens : List Token) : Option (Value × List Token) :=
    match parseArith tokens with
    | some (left, Token.eq :: rest) =>
      match parseArith rest with
      | some (right, rest') => some (Value.bool (equal left right), rest')
      | none => some (left, rest)
    | some (left, Token.ne :: rest) =>
      match parseArith rest with
      | some (right, rest') => some (Value.bool (!equal left right), rest')
      | none => some (left, rest)
    | some (left, Token.lt :: rest) =>
      match parseArith rest with
      | some (right, rest') => some (Value.bool (compare left right == Ordering.lt), rest')
      | none => some (left, rest)
    | some (left, Token.le :: rest) =>
      match parseArith rest with
      | some (right, rest') => some (Value.bool (compare left right != Ordering.gt), rest')
      | none => some (left, rest)
    | some (left, Token.gt :: rest) =>
      match parseArith rest with
      | some (right, rest') => some (Value.bool (compare left right == Ordering.gt), rest')
      | none => some (left, rest)
    | some (left, Token.ge :: rest) =>
      match parseArith rest with
      | some (right, rest') => some (Value.bool (compare left right != Ordering.lt), rest')
      | none => some (left, rest)
    | r => r

  partial def parseAnd (tokens : List Token) : Option (Value × List Token) :=
    match parseCompare tokens with
    | some (left, Token.amp :: rest) =>
      match parseAnd rest with
      | some (right, rest') =>
        if isTrue left && isTrue right then
          some (left, rest')
        else
          some (Value.int 0, rest')
      | none => some (left, rest)
    | r => r

  partial def parseOr (tokens : List Token) : Option (Value × List Token) :=
    match parseAnd tokens with
    | some (left, Token.pipe :: rest) =>
      match parseOr rest with
      | some (right, rest') =>
        if isTrue left then
          some (left, rest')
        else
          some (right, rest')
      | none => some (left, rest)
    | r => r

  partial def parseExpr (tokens : List Token) : Option (Value × List Token) :=
    parseOr tokens
end

def evaluate (s : String) : Option Value :=
  let tokens := tokenize s
  match parseExpr tokens with
  | some (v, ts) =>
    match ts with
    | [] => some v
    | [Token.eof] => some v
    | _ => none
  | none => none

def formatValue (v : Value) : String :=
  match v with
  | Value.int i => toString i
  | Value.str s => s
  | Value.bool true => "1"
  | Value.bool false => "0"

end Lentils.Expr.Logic
