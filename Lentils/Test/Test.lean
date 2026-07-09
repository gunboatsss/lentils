/-
Test — IO wrapper for the `test` utility. 0BSD
-/

import Lentils.Test.Logic

namespace Lentils.Test

open Logic

-- Collect all file paths from file-test expressions in the AST.
partial def collectPaths (e : Expr) : List String :=
  match e with
  | Expr.fileIsFile path => [path]
  | Expr.fileIsDir path => [path]
  | Expr.fileExists path => [path]
  | Expr.fileNotEmpty path => [path]
  | Expr.fileReadable path => [path]
  | Expr.fileWritable path => [path]
  | Expr.fileExecutable path => [path]
  | Expr.notExpr e' => collectPaths e'
  | Expr.andExpr e1 e2 => collectPaths e1 ++ collectPaths e2
  | Expr.orExpr e1 e2 => collectPaths e1 ++ collectPaths e2
  | _ => []

-- Build a StatContext from Lean's native System.FilePath.metadata.
def buildContext (md : IO.FS.Metadata) : StatContext :=
  let isReg := md.type == IO.FS.FileType.file
  let isDir := md.type == IO.FS.FileType.dir
  { pathExists := true
  , isFile := isReg
  , isDir := isDir
  , size := md.byteSize
  , readable := true
  , writable := true
  , executable := true
  }

-- Stat each unique path using Lean's native metadata, building lookup pairs.
partial def statPathList : List String → List (String × StatContext) → IO (List (String × StatContext))
  | [], acc => return acc
  | p :: rest, acc => do
    try
      let md ← (System.FilePath.mk p).metadata
      statPathList rest ((p, buildContext md) :: acc)
    catch _ =>
      statPathList rest ((p, defaultCtx) :: acc)

-- Build a lookup function from path → StatContext by statting every unique path
-- found in the expression. Unseen paths fall back to defaultCtx.
def buildLookup (paths : List String) : IO (String → StatContext) := do
  let ctxs ← statPathList paths []
  return λ p =>
    match ctxs.find? (λ (q, _) => q == p) with
    | some (_, c) => c
    | none => defaultCtx

-- Strip trailing "]" when invoked via the `[` form.
def run (args : List String) : IO UInt32 := do
  let cleaned := match args.reverse with
    | "]" :: rest => rest.reverse
    | _ => args
  match parseArgs cleaned with
  | some e => do
    let paths := collectPaths e
    let lookup ← buildLookup paths
    return boolToExit (eval lookup e)
  | none => return exitFalse

end Lentils.Test
