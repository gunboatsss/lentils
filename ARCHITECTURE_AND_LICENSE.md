# lentils: license & architecture plan

This document is the hand-off analysis for a Smithers (or other agent) workflow.
It covers (1) the license strategy and (2) the common-code / FFI architecture for
a Lean 4 port of Unix coreutils.

---

## 1. License strategy

### 1.1 Major coreutils reimplementations

| Project          | License                              | What it means for us |
|------------------|--------------------------------------|----------------------|
| GNU coreutils    | GPLv3+ (most files)                  | **Highest risk.** Any code copied or translated from GNU coreutils forces this project to become GPLv3+. Merely reading the source is legal, but reimplementing the same utility while looking at GPL source creates real "substantial similarity" / derivative-work risk. Treat as off-limits for direct source inspection. |
| BusyBox          | GPLv2                                | Same issue as GNU coreutils. Its single-binary / multi-applet model is architecturally interesting, but its implementation is GPL. Do not read BusyBox source while writing equivalents. |
| ToyBox           | 0BSD (public-domain-ish)             | Very low risk. You may read it for ideas and even adapt code, but if you translate ToyBox code into Lean the result is a derivative of ToyBox. 0BSD does not require attribution, but add a note in `THIRD_PARTY_NOTICES.md` anyway for cleanliness. |
| uutils (Rust)    | MIT                                  | Low risk. Reading is fine. If you port uutils code, you must preserve the MIT copyright notice. MIT is compatible with any permissive license we choose. |

### 1.2 Core legal point

Copyright protects **expression**, not **ideas** or **interfaces**.  Interface
specifications (POSIX, `man 2 open`, etc.) are facts and are not copyrighted.
The danger is copying expressive choices: variable names, comments, control
flow, file structure, or clever helper functions.

The cleanest way to avoid contamination is therefore **not the license you
pick** — it is the **provenance discipline**:

* Write from POSIX / The Open Group Base Specifications and system man pages.
* Do **not** read GPL source (GNU coreutils, BusyBox) while implementing the
  same utility.
* If you read permissive source (ToyBox, uutils), document it.
* If you copy/adapt permissive source, keep the required attribution.

### 1.3 Recommended license

**Use 0BSD** (BSD Zero Clause License), the same license ToyBox uses.

Rationale:

* It is the most permissive license possible — no attribution requirement, no
  copyleft, no patent clause, no conditions at all.
* It matches ToyBox, the coreutils reimplementation we are most aligned with
  architecturally (single binary + applets).  This makes it natural to reference
  ToyBox for ideas without license friction.
* It is compatible with everything: MIT code (uutils) can be incorporated as
  long as the MIT notice is preserved in `THIRD_PARTY_NOTICES.md`.
* If, against policy, any GPL code were ever introduced, the project would have
  to be relicensed to GPL. A permissive starting point makes that problem easy
  to see and prevent.

### 1.4 Required files

* `LICENSE` — 0BSD full text.
* `PROVENANCE.md` — list of every non-trivial reference used (specs, man pages,
  permissive codebases).  State explicitly that GPL implementations were not
  used as source material.
* `THIRD_PARTY_NOTICES.md` — attribution for any borrowed MIT/0BSD snippets.

---

## 2. Common code / FFI architecture

### 2.1 High-level decision: single binary + symlinks

Build **one** Lean executable (`coreutils`) and dispatch based on `argv[0]`.
Install creates symlinks such as `cat -> coreutils`, `echo -> coreutils`, etc.

Why:

* A minimal Lean "hello world" binary is **~4.1 MB unstripped / ~2.6 MB
  stripped** on this machine. 100 separate `lean_exe` targets would duplicate
  the Lean runtime ~100 times (≈ 260 MB+). A single binary amortizes that cost.
* Lake only needs one `lean_exe` target; CI only builds one artifact.
* The BusyBox/ToyBox model is well understood by packagers.

Trade-off:

* You cannot install just one utility without the whole binary. For a
  coreutils port this is normal and acceptable.

### 2.2 Lean runtime reality

You **cannot** remove the Lean runtime for a Lean program. Even a trivial
`main` links the GC, allocator, task manager, panic machinery, etc. Accept this
overhead and make the binary as small as reasonable by stripping release
builds. Do not waste effort trying to write utilities in C to avoid the runtime.

### 2.3 Repository layout

```text
lentils/
├── LICENSE
├── PROVENANCE.md
├── lakefile.lean          # switched from .toml to support extern_lib
├── LeanCoreutils.lean     # re-export public API
├── Main.lean              # single dispatcher + main
├── LeanCoreutils/
│   ├── Common/
│   │   ├── Bytes.lean     # verified byte ops (split, count, take/drop)
│   │   ├── Lines.lean     # verified line split/join roundtrip
│   │   ├── Args.lean      # POSIX-style option parser
│   │   ├── Errors.lean    # program_name + strerror, exit codes
│   │   ├── IO/
│   │   │   ├── Fd.lean    # raw file-descriptor FFI (unverified)
│   │   │   ├── Stat.lean  # stat/lstat + permission bits (unverified)
│   │   │   └── Buffer.lean# buffered byte I/O helpers (unverified)
│   │   └── Signal.lean    # sigaction / signal disposition FFI (unverified)
│   ├── Cat/
│   │   ├── Logic.lean     # pure: processBytes + identity proof
│   │   └── Cat.lean       # IO wrapper using Fd + Cat.Logic
│   ├── Echo/
│   │   ├── Logic.lean     # pure: formatArgs + correctness proof
│   │   └── Echo.lean      # IO wrapper
│   ├── Sort/
│   │   ├── Logic.lean     # pure: sortLines + Sorted + Perm proofs
│   │   └── Sort.lean      # IO wrapper
│   └── Wc/
│       ├── Logic.lean     # pure: countLines + correctness proof
│       └── Wc.lean        # IO wrapper
├── c/
│   └── coreutils.c        # C wrappers around libc / syscalls
├── tests/
│   └── differential/      # golden tests vs host binaries
└── scripts/
    └── install-symlinks.sh
```

`LeanCoreutils` is a library. `Main` only dispatches. Each utility lives in its
own namespace and exposes `run : List String → IO UInt32`.

### 2.3.1 Formal verification: pure/IO split

Lean is both a programming language and a theorem prover. We exploit this by
separating each utility into a **pure, verified core** (`Logic.lean`) and a
**thin, unverified IO wrapper** (`<Util>.lean`).

**Rules:**

* `Logic.lean` files contain only pure functions — no `IO`, no `FFI`, no
  side effects. These are formally verified with `theorem` / `example`.
* `<Util>.lean` files are thin wrappers that read input via FFI, call the pure
  logic, and write output via FFI. They are covered by differential tests.
* **No `sorry` or `admit`** is allowed in any `Logic.lean`. A CI script enforces
  this:
  ```bash
  ! grep -rn 'sorry\|admit' LeanCoreutils/**/Logic.lean
  ```
* A failed proof is a **compile error** (`lake build` fails), so verification
  is enforced at build time, not just in CI.

**Properties to prove per utility:**

| Utility | Pure function | Property |
|---------|-------------|----------|
| `cat` | `processBytes : ByteArray → ByteArray` | identity: `processBytes x = x` |
| `echo` | `formatArgs : List String → String` | output = args joined by space + newline |
| `wc -l` | `countLines : ByteArray → Nat` | count = number of `\n` bytes |
| `sort` | `sortLines : List String → List String` | `Sorted` output ∧ `Perm` input output |
| `uniq` | `dedupAdjacent : List α → List α` | no adjacent duplicates ∧ `Perm` input output |
| `head -n N` | `takeLines : Nat → List String → List String` | `length ≤ N` (or input length) |
| `tail -n N` | `dropLines : Nat → List String → List String` | output = last min(N, len) lines |
| `cut -f N` | `extractFields : Nat → List String → List String` | each output line has ≤ N fields |
| `tr set1 set2` | `mapChars : Char × Char → String → String` | mapping correct per position |
| `cp` (core) | `copyBytes : ByteArray → ByteArray` | identity: `copyBytes x = x` |

For `sort`, prefer Mathlib's `List.mergeSort` which already has sortedness and
permutation proofs — we get verification for free.

**Verified common primitives:**

* `Common/Bytes.lean` — byte-level ops (split on `\n`, count, take/drop) with
  proofs.
* `Common/Lines.lean` — `splitLines : ByteArray → List ByteArray` with proof
  that `joinLines (splitLines x) = x` (roundtrip identity).

IO-heavy utilities (`ls`, `mv`, `rm`, `mkdir`, `ln`) are mostly syscalls and
have minimal provable logic. `cp` has a provable byte-copy core. These rely on
differential testing rather than proofs.

### 2.4 Build configuration (`lakefile.lean`)

Switch from `lakefile.toml` to `lakefile.lean` so we can declare an
`extern_lib` target that builds a static C archive:

```lean
import Lake
import Lake.Config.InstallPath

open System Lake DSL

package «lentils» where
  version := v!"0.1.0"

lean_lib LeanCoreutils

extern_lib coreutilsSupport (pkg) := do
  let src := pkg.dir / "c" / "coreutils.c"
  let o   := pkg.buildDir / "c" / "coreutils.o"
  let lib := pkg.buildDir / "c" / "libcoreutils_support.a"
  Job.async do
    let some sysroot ← Lake.findLeanSysroot?
      | error "could not find Lean sysroot"
    let lean ← LeanInstall.get sysroot
    -- Use the system C compiler; bundled leanc intentionally lacks system
    -- headers, but we need Lean's include path.
    compileO o src (lean.cFlags ++ #["-I", lean.includeDir.toString]) "cc"
    compileStaticLib lib #[o]
    return lib

@[default_target] lean_exe coreutils where
  root := `Main
```

The `extern_lib` target is automatically linked into the single executable.

### 2.5 Dispatcher (`Main.lean`)

```lean
import LeanCoreutils

def dispatch (prog : String) (args : List String) : IO UInt32 :=
  match prog with
  | "cat"   => LeanCoreutils.Cat.run args
  | "echo"  => LeanCoreutils.Echo.run args
  | "true"  => return 0
  | "false" => return 1
  | "coreutils" | "lentils" =>
      match args with
      | [] => do IO.println "Usage: coreutils <applet> ..."; return 0
      | applet :: rest => dispatch applet rest
  | _ => do
      IO.eprintln s!"{prog}: unknown applet"
      return 127

def main (args : List String) : IO UInt32 :=
  let prog := (FilePath.mk (args.headD "lentils")).fileName.getD "lentils"
  dispatch prog args.tail
```

This supports both symlink invocation (`cat file`) and explicit invocation
(`coreutils cat file`).

### 2.6 Error reporting (`Common/Errors.lean`)

All utilities should report errors as `program: file: message` on stderr and
return a non-zero `UInt32` exit code.  Provide helpers:

```lean
def errorMessage (prog file? msg : String) : String :=
  match file? with
  | none   => s!"{prog}: {msg}"
  | some f => s!"{prog}: {f}: {msg}"

def exitError (prog : String) (file? : Option String) (msg : String) : IO UInt32 := do
  IO.eprintln (errorMessage prog file? msg)
  return 1
```

Use `IO.userError` or the FFI wrappers to turn `errno` into a message; do not
use `panic` for user-facing failures.

### 2.7 Argument parsing (`Common/Args.lean`)

Lean 4.31 has no built-in `getopt`. Implement a tiny reusable parser.  A
utility declares its options and operands:

```lean
structure Flag where
  short? : Option Char
  long?  : Option String
  hasArg : Bool

def parseOptions (flags : List Flag) (args : List String) (acc : α)
    (handle : String → α → Except String α)
    : Except String (α × List String) := ...
```

For the first utilities (`echo`, `cat`, `true`, `false`, `pwd`) a simple
hand-rolled loop is enough.  Do not pull in a third-party CLI library until the
benefit outweighs the dependency/license review cost.

### 2.8 When to use Lean's `IO.FS` vs raw FFI

Lean already wraps a lot of useful functionality.  Use these first:

* `IO.FS.Handle.mk`, `.read`, `.write`, `.flush`, `.getLine`, `.putStr` for
  normal buffered file I/O.
* `IO.getStdin` / `IO.getStdout` / `IO.getStderr` and `FS.Stream` for stdio.
* `System.FilePath` for path manipulation.
* `IO.FS.metadata` / `symlinkMetadata` for times, size, type, nlink.

Drop to the C wrappers in `c/coreutils.c` when you need:

* fine-grained `open(2)` flags (`O_NOFOLLOW`, `O_DIRECTORY`, `O_CLOEXEC`,
  `O_APPEND`, custom modes),
* permission bits, uid/gid, device/inode, hard-link count (`stat` / `lstat`),
* `chmod`, `chown`, `utimensat`,
* `symlink`, `readlink`, `link`, `rename`, `mkdir`, `rmdir`, `unlink`,
* `getcwd`, `dup2`, pipes, `fcntl`,
* signal dispositions (`sigaction`, `signal`),
* exact unbuffered reads/writes for utilities like `dd`.

### 2.9 FFI pattern

Lean side:

```lean
namespace LeanCoreutils.IO

@[extern "lean_coreutils_open"] opaque openFile (path : @& String) (flags : UInt32) (mode : UInt32) : IO UInt32
@[extern "lean_coreutils_close"] opaque closeFd (fd : UInt32) : IO Unit
@[extern "lean_coreutils_read"]  opaque readBytes (fd : UInt32) (n : USize) : IO ByteArray
@[extern "lean_coreutils_write"] opaque writeBytes (fd : UInt32) (buf : @& ByteArray) : IO USize

end LeanCoreutils.IO
```

C side (`c/coreutils.c`):

```c
#include <lean/lean.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>

static inline lean_object *io_err(void) {
    return lean_io_result_mk_error(lean_mk_string(strerror(errno)));
}

LEAN_EXPORT lean_object *lean_coreutils_open(b_lean_obj_arg path,
                                             uint32_t flags,
                                             uint32_t mode,
                                             lean_object *w) {
    int fd = open(lean_string_cstr(path), (int)flags, (mode_t)mode);
    if (fd < 0) return io_err();
    return lean_io_result_mk_ok(lean_box((uint32_t)fd));
}

LEAN_EXPORT lean_object *lean_coreutils_close(uint32_t fd, lean_object *w) {
    if (close((int)fd) < 0) return io_err();
    return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_object *lean_coreutils_read(uint32_t fd, size_t n, lean_object *w) {
    if (n == 0) return lean_io_result_mk_ok(lean_alloc_sarray(1, 0, 0));
    uint8_t *tmp = (uint8_t*)malloc(n);
    if (tmp == NULL) return io_err();
    ssize_t r = read((int)fd, tmp, n);
    if (r < 0) { free(tmp); return io_err(); }
    lean_object *ba = lean_alloc_sarray(1, (size_t)r, (size_t)r);
    memcpy(lean_sarray_cptr(ba), tmp, (size_t)r);
    free(tmp);
    return lean_io_result_mk_ok(ba);
}

LEAN_EXPORT lean_object *lean_coreutils_write(uint32_t fd,
                                              b_lean_obj_arg ba,
                                              lean_object *w) {
    size_t n = lean_sarray_size(ba);
    ssize_t r = write((int)fd, lean_sarray_cptr(ba), n);
    if (r < 0) return io_err();
    return lean_io_result_mk_ok(lean_box((uint32_t)r));
}
```

Rules:

* All C symbols are `LEAN_EXPORT` so the linker sees them.
* Names must match the `extern` string exactly.
* `IO α` functions take a trailing `lean_object *w` and return
  `lean_io_result_mk_ok(...)` or `lean_io_result_mk_error(...)`.
* Unboxed return values are `lean_box`-ed. `Unit` is `lean_box(0)`.
* `String` is `lean_object*`; use `lean_string_cstr` for the UTF-8 bytes.
* `ByteArray` is a scalar array; use `lean_sarray_size` / `lean_sarray_cptr`.

### 2.10 Opaque C structs (e.g., `stat`)

For complex C structs, return an opaque external object rather than a raw byte
array.  Example sketch for `stat`:

```c
static lean_external_class *g_stat_class = NULL;

typedef struct { struct stat st; } lean_stat;

static void stat_finalize(void *p) { free(p); }
static void stat_foreach(void *p, b_lean_obj_arg f) {}

LEAN_EXPORT lean_object *lean_coreutils_lstat(b_lean_obj_arg path, lean_object *w) {
    if (!g_stat_class) g_stat_class = lean_register_external_class(stat_finalize, stat_foreach);
    lean_stat *s = (lean_stat*)malloc(sizeof(lean_stat));
    if (lstat(lean_string_cstr(path), &s->st) < 0) { free(s); return io_err(); }
    return lean_io_result_mk_ok(lean_alloc_external(g_stat_class, s));
}

LEAN_EXPORT uint32_t lean_coreutils_stat_mode(b_lean_obj_arg s) {
    return ((lean_stat*)lean_to_external(s)->m_data)->st.st_mode;
}
```

Lean side:

```lean
opaque FileStat : Type := Unit

@[extern "lean_coreutils_lstat"] opaque lstat (path : @& String) : IO FileStat
@[extern "lean_coreutils_stat_mode"] opaque statMode : FileStat → UInt32
```

The Lean GC will call the registered finalizer when the `FileStat` is no longer
referenced.

### 2.11 Signal handling

Lean 4.31 does not expose signal handlers.  Provide a minimal C wrapper that
only changes the signal disposition to `SIG_IGN`, `SIG_DFL`, or (later) a
simple flag-setting handler.  Do **not** call back into Lean from a signal
handler.

```lean
@[extern "lean_coreutils_ignore_sigpipe"] opaque ignoreSigpipe : IO Unit
```

```c
#include <signal.h>
LEAN_EXPORT lean_object *lean_coreutils_ignore_sigpipe(lean_object *w) {
    signal(SIGPIPE, SIG_IGN);
    return lean_io_result_mk_ok(lean_box(0));
}
```

For utilities that need cleanup on `SIGINT` (e.g., `cp` with temp files), keep
temporary state in C globals and install a C handler that removes them; do not
try to run Lean code in the handler.

### 2.12 Path representation caveat

`System.FilePath` is a UTF-8 `String`.  Unix paths are byte strings and may not
be valid UTF-8.  For an MVP this is acceptable; document the limitation.  If a
utility needs exact byte paths (e.g., `ls` on arbitrary filesystems), pass a
`ByteArray` to the FFI instead of a `String`.

### 2.13 Install script (`scripts/install-symlinks.sh`)

```sh
#!/bin/sh
set -e
BIN_DIR="${1:-$PWD/.lake/build/bin}"
INSTALL_DIR="${2:-$HOME/.local/bin}"

mkdir -p "$INSTALL_DIR"

for app in cat echo true false pwd; do
    ln -sf "$BIN_DIR/coreutils" "$INSTALL_DIR/$app"
done

echo "Installed coreutils applets to $INSTALL_DIR"
```

---

## 3. Suggested first milestone (validation task)

Implement the smallest possible end-to-end slice to prove the architecture:

1. `LICENSE`, `PROVENANCE.md` in place.
2. `lakefile.lean` with the `extern_lib` target above.
3. C wrappers for `open`, `close`, `read`, `write`.
4. `LeanCoreutils.Common.IO.Fd` with the extern declarations.
5. `LeanCoreutils.Common.Bytes` + `Common.Lines` with verified primitives.
6. `LeanCoreutils.Cat.Logic` — pure `processBytes` + identity proof.
7. `LeanCoreutils.Cat` (IO wrapper) using the raw FD wrappers + `Cat.Logic`.
8. `Main.lean` dispatcher.
9. `scripts/install-symlinks.sh`.
10. Tests run via `lake test` comparing against `/bin/cat`:
    * copy a text file,
    * copy a binary file,
    * read from stdin,
    * multiple files,
    * missing file returns non-zero and prints `cat: ...: No such file or directory`.
11. No-`sorry` check passes.

Once `cat` works (logic proved + wrapper tested), the same pattern applies to
 every other utility: `Logic.lean` (proof) → `<Util>.lean` (IO) → tests.

---

## 4. Testing strategy

### 4.1 Three-layer verification

Every utility is validated at three layers:

1. **Formal proofs** (compile-time): `Logic.lean` theorems checked by `lake
   build`. A failed proof is a compile error. No `sorry`/`admit` allowed.
2. **Differential tests** (runtime): compare stdout, stderr, exit code, and
   resulting filesystem state against the host's coreutils.
3. **Edge-case tests**: empty files, no trailing newline, binary zeros, very
   long lines, unreadable files, closed stdout (SIGPIPE), invalid-UTF-8 byte
   sequences, deeply nested directories, circular symlinks.

### 4.2 Sandbox for destructive utilities

Utilities like `rm`, `mv`, `cp`, `mkdir`, `rmdir`, `ln` modify the filesystem.
Running them against the real FS is dangerous and non-deterministic. Use
**bubblewrap (`bwrap`)** to create an isolated sandbox for each test case.

**Why bwrap:** it creates a throwaway mount namespace with a tmpfs root. Even
`rm -rf /` inside the sandbox only affects the tmpfs. No cleanup needed — the
sandbox vanishes when the process exits. No root required.

**Sandbox wrapper** (`tests/sandbox/run-in-sandbox.sh`):

```sh
#!/bin/sh
# run-in-sandbox.sh <fixture-dir> <size-bytes> -- <command...>
# - Mounts fixture read-only at /fixture (EROFS if the util tries to write it)
# - Creates a writable tmpfs at /sandbox (optionally size-limited for ENOSPC)
# - Copies the fixture into /sandbox so the util can modify a copy
# - --die-with-parent ensures cleanup on crash

FIXTURE="$1"; shift
SIZE="$1"; shift
[ "$1" = "--" ] && shift

bwrap \
  --unshare-all \
  --die-with-parent \
  --dev /dev \
  --proc /proc \
  --ro-bind /bin /bin \
  --ro-bind /lib /lib \
  --ro-bind /lib64 /lib64 \
  --size "$SIZE" --tmpfs /sandbox \
  --ro-bind "$FIXTURE" /fixture \
  -- /bin/sh -c 'cp -a /fixture/. /sandbox/ && exec "$@"' -- "$@"
```

**Failure-injection patterns:**

| Condition | bwrap flag | What it simulates |
|-----------|-----------|-------------------|
| Read-only FS | `--ro-bind $FIXTURE /fixture` | EROFS on write attempts |
| Disk full | `--size 8192 --tmpfs /sandbox` | ENOSPC after N bytes |
| No /dev | omit `--dev /dev` | ENXIO on device access |
| No network | `--unshare-net` (included in `--unshare-all`) | ENETUNREACH |
| Empty sandbox | `--tmpfs /sandbox` only | ENOENT on all paths |

**Differential test flow for destructive utilities:**

```
for each test case:
  1. snapshot fixture state (find + sha256 of all files)
  2. run HOST binary in sandbox A → capture stdout/stderr/exit + FS state after
  3. run OUR binary  in sandbox B → capture stdout/stderr/exit + FS state after
  4. diff stdout, stderr, exit code
  5. diff FS state (file tree + contents)
  6. pass iff all match
```

This catches bugs where the *output* matches but the *side effect* differs
(e.g., `rm` prints the right message but doesn't actually delete the file, or
deletes the wrong file).

### 4.3 Test fixtures

`tests/fixtures/` contains read-only directory trees used as sandbox seeds:

```text
tests/fixtures/
├── basic/          # simple files: a.txt, b.txt, empty.txt
├── nested/         # dir tree: a/b/c/d.txt, e/f.txt
├── symlinks/       # valid, dangling, circular symlinks
├── permissions/    # 000, 444, 600, 755 mode files
├── special/        # empty, binary zeros, no-newline, very-long-line
└── destructive/    # pre-populated state for rm/mv/cp tests
```

Fixtures are committed to the repo (they're small, static, and deterministic).

### 4.4 Other tests

* **Property tests**: for utilities like `cat`, idempotence (`cat ∘ cat = cat`)
  and concatenation order. For `sort`, idempotence (`sort ∘ sort = sort`).
* **Build tests**: CI must build on a clean checkout with `lake build` and the
  install script must create a working `cat` symlink.
* **License audit**: a CI step (or Smithers task) scans for copyleft code and
  rejects changes that add GPL/AGPL/CC-NC/etc. without explicit relicensing
  approval.
* **No-sorry audit**: `! grep -rn 'sorry\|admit' LeanCoreutils/**/Logic.lean`.

---

## 5. Safety checklist before considering the architecture done

* [ ] `LICENSE` is 0BSD.
* [ ] `PROVENANCE.md` exists and explicitly disclaims use of GPL source.
* [ ] `lakefile.lean` builds the C support library and the single executable.
* [ ] A single `lake build` produces `.lake/build/bin/coreutils`.
* [ ] `scripts/install-symlinks.sh` creates symlinks that dispatch correctly.
* [ ] Running `coreutils cat file` and `cat file` (via symlink) produce the same
      output.
* [ ] C wrappers handle `errno` and return sensible `IO.Error` messages.
* [ ] No `panic` is used for user-facing error paths.
* [ ] The release binary is stripped; size is measured and recorded.
* [ ] Test driver runs and at least one host-vs-our comparison passes.
* [ ] Every utility has a `Logic.lean` with formal proofs (no `IO`/`FFI`).
* [ ] No `sorry` or `admit` in any `Logic.lean` (CI-enforced).
* [ ] `lake build` succeeds — failed proofs are compile errors.
* [ ] Pure/IO split is clean: all side effects confined to `<Util>.lean` wrappers.
* [ ] Destructive utilities tested inside a bwrap sandbox (no real FS risk).
* [ ] FS-state diff included in differential tests (not just stdout/stderr).
* [ ] Failure-injection tests cover EROFS, ENOSPC, ENOENT, EACCES.
* [ ] Test fixtures are committed and deterministic.
