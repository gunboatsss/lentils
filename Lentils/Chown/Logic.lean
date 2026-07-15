/-
Chown.Logic — Pure logic for the `chown` utility.
0BSD

Contains only pure functions: argument parsing. No IO is performed here.
All filesystem interaction lives in `chown.lean` via the C FFI `chown(2)`.

Provenance: POSIX.1-2017, Section "chown — change file owner and group".
No GPL source was consulted.
-/

namespace Lentils.Chown.Logic

/--
Options controlling `chown` behaviour.
-/
structure Options where
  verbose : Bool := false
  recursive : Bool := false
  deriving Repr, BEq, DecidableEq

/--
A parsed owner:group specification.

Either field being empty means "don't change".
-/
structure OwnerGroup where
  owner : String
  group : String
  deriving Repr, BEq, DecidableEq

/--
Parse an `owner[:group]` spec string.

  parseOwnerGroup "alice"       = some { owner := "alice", group := "" }
  parseOwnerGroup "alice:staff" = some { owner := "alice", group := "staff" }
  parseOwnerGroup ":staff"      = some { owner := "", group := "staff" }
  parseOwnerGroup ""             = none
-/
def parseOwnerGroup (s : String) : Option OwnerGroup :=
  if s.isEmpty then none
  else
    match s.splitOn ":" with
    | [owner] => some { owner, group := "" }
    | [owner, group] => some { owner, group }
    | _ => none  -- multiple colons

/--
Parse `chown` arguments into `(options, spec, files)`.

  chown [OPTIONS] OWNER[:GROUP] FILE...
-/
def parseArgs (args : List String) : Options × Option OwnerGroup × List String :=
  let rec go (remaining : List String) (opts : Options) (files : List String)
      : Options × Option OwnerGroup × List String :=
    match remaining with
    | [] => (opts, none, files.reverse)
    | "--" :: rest => (opts, none, files.reverse ++ rest)
    | "-v" :: rest => go rest { opts with verbose := true } files
    | "--verbose" :: rest => go rest { opts with verbose := true } files
    | "-R" :: rest => go rest { opts with recursive := true } files
    | "--recursive" :: rest => go rest { opts with recursive := true } files
    | s :: rest =>
      -- First non-flag token is the owner:group spec
      if s.startsWith "-" && s != "-" then
        (opts, none, files.reverse)
      else
        -- s is the owner:group spec, rest are files
        let spec := parseOwnerGroup s
        (opts, spec, files.reverse ++ rest)
  go args {} []

def optionsOf (p : Options × Option OwnerGroup × List String) : Options := p.1
def specOf (p : Options × Option OwnerGroup × List String) : Option OwnerGroup := p.2.1
def filesOf (p : Options × Option OwnerGroup × List String) : List String := p.2.2

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- Parse simple owner. -/
theorem parse_owner :
  parseOwnerGroup "alice" = some { owner := "alice", group := "" } := by
  native_decide

/-- Parse owner:group. -/
theorem parse_owner_group :
  parseOwnerGroup "alice:staff" = some { owner := "alice", group := "staff" } := by
  native_decide

/-- Parse :group. -/
theorem parse_group_only :
  parseOwnerGroup ":staff" = some { owner := "", group := "staff" } := by
  native_decide

/-- Empty string is none. -/
theorem parse_empty :
  parseOwnerGroup "" = none := by
  native_decide

end Lentils.Chown.Logic
