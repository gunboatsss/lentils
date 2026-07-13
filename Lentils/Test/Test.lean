/-
Test — IO wrapper for the `test` utility. 0BSD
-/

import Lentils.Test.Logic

namespace Lentils.Test

open Logic

-- POSIX access(2) mode bits.  R_OK=4, W_OK=2, X_OK=1 (from <unistd.h>).
private def rOK : UInt32 := 4
private def wOK : UInt32 := 2
private def xOK : UInt32 := 1

-- access(2): returns 1 if the requested mode is permitted for `path`,
-- 0 otherwise (including EACCES / ENOENT).  This is the IO-layer wiring
-- that populates StatContext.readable / .writable / .executable, which
-- Lean's native IO.FS.Metadata does not surface.
@[extern "lean_coreutils_access"]
opaque access (path : String) (mode : UInt32) : IO UInt32

def accessR (path : String) : IO Bool := do return (← access path rOK) == 1
def accessW (path : String) : IO Bool := do return (← access path wOK) == 1
def accessX (path : String) : IO Bool := do return (← access path xOK) == 1

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

-- Build a StatContext from Lean's native System.FilePath.metadata
-- combined with access(2) permission checks.
def buildContext (path : String) (md : IO.FS.Metadata) : IO StatContext := do
  let isReg := md.type == IO.FS.FileType.file
  let isDir := md.type == IO.FS.FileType.dir
  let r ← accessR path
  let w ← accessW path
  let x ← accessX path
  let ctx : StatContext :=
    { pathExists := true
    , isFile := isReg
    , isDir := isDir
    , size := md.byteSize
    , readable := r
    , writable := w
    , executable := x
    }
  return ctx

-- Stat each unique path using Lean's native metadata, building lookup pairs.
partial def statPathList : List String → List (String × StatContext) → IO (List (String × StatContext))
  | [], acc => return acc
  | p :: rest, acc => do
    try
      let md ← (System.FilePath.mk p).metadata
      let ctx ← buildContext p md
      statPathList rest ((p, ctx) :: acc)
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
