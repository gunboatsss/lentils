# lentils Implementation Roadmap

## Finish line: self-replicating build

The project is complete when lentils replaces every command the build
pipeline calls. Given a system with these **external prerequisites**, 
`lake build` succeeds using ONLY our utilities:

| Prerequisite | Why it's external |
|-------------|-------------------|
| Linux kernel | System calls |
| libc (glibc/musl) | `@[extern]` linkage, present on every Linux system |
| C compiler (gcc/clang) | `c/coreutils.c` must be compiled for FFI |
| Lean 4 toolchain (lean, lake) | Binary distribution; Lean is self-hosting separately |
| Shell (dash/bash/ash) | NOT a coreutil — separate project |

### Out of scope (use system versions)

`sh`, `bash`, `dash`, `ash` — shells are a separate project (ToyBox's `toysh` has been years in development)

`vi`, `ed`, `nano` — text editors

`gcc`, `cc`, `clang`, `as`, `ld`, `make`, `cmake` — compiler toolchain

`glibc`, `musl` — C library

`lean`, `lake`, `leanpkg` — Lean toolchain (binary distribution, Lean is self-hosting)

`mount`, `umount`, `fdisk`, `init`, `switch_root` — system boot. The system is already booted.

`wget`, `curl`, `ftpget` — network. Dependencies are pre-downloaded.

`gzip`, `tar`, `xz`, `bzcat`, `zcat` — archives. Pre-extract before build.

### In scope: what `lake build` calls

The build pipeline (`lake build` → `cc -c c/coreutils.c` → `ar` → `ld` → binary) exercises:

```
cat, echo, true, false, test, [, printf         # shell conditionals
cp, mv, rm, mkdir, rmdir, ln, chmod, ls        # file operations
grep, sed, head, tail, sort, uniq, tr, cut, wc # text processing
find, xargs, dirname, basename, pwd            # path operations
env, uname                                     # environment / system
tee                                            # output duplication (build logs)
```

## Priority Tiers

### Tier 1: Core (implement first)

| Utility | POSIX spec | Formal proof | Sandbox tests | Sandbox risk |
|---------|-----------|-------------|---------------|--------------|
| `cat` | [cat.md](posix/cat.md) | ✅ identity + idempotence | ✅ 5 tests | 🟢 read-only |
| `true` | [true.md](posix/true.md) | ✅ exitCode_is_zero | ✅ 3 tests | 🟢 no IO |
| `false` | [false.md](posix/false.md) | ⬜ exitCode = 1 | ⬜ | 🟢 no IO |
| `echo` | [echo.md](posix/echo.md) | ⬜ | ⬜ | 🟢 stdout only |

### Tier 2: Simple (stdout/stdin only, no FS modification)

| Utility | POSIX spec | Notes |
|---------|-----------|-------|
| `pwd` | [pwd.md](posix/pwd.md) | `getcwd` syscall |
| `basename` | [basename.md](posix/basename.md) | String manipulation |
| `dirname` | [dirname.md](posix/dirname.md) | String manipulation |
| `env` | — | `execvp` wrapper |
| `printf` | — | Format strings to stdout |
| `sleep` | — | `nanosleep` syscall |
| `wc` | [wc.md](posix/wc.md) | Count lines/words/bytes |
| `head` | [head.md](posix/head.md) | First N lines |
| `tail` | [tail.md](posix/tail.md) | Last N lines |
| `cut` | [cut.md](posix/cut.md) | Field extraction |
| `tr` | [tr.md](posix/tr.md) | Character translation |
| `tee` | — | Tee stdout to files |
| `sort` | [sort.md](posix/sort.md) | Use Mathlib `mergeSort` (pre-proved) |
| `uniq` | [uniq.md](posix/uniq.md) | Adjacent dedup |

### Tier 3: File system operations (destructive — require bwrap sandbox)

Each utility runs inside a bwrap sandbox with tmpfs root. Differential tests
compare both stdout/stderr/exit AND filesystem state (file tree + sha256).

| Utility | POSIX spec | Sandbox risk |
|---------|-----------|--------------|
| `cp` | [cp.md](posix/cp.md) | 🟡 Creates/overwrites |
| `mv` | [mv.md](posix/mv.md) | 🔴 Moves/overwrites |
| `rm` | [rm.md](posix/rm.md) | 🔴 Deletes files |
| `mkdir` | [mkdir.md](posix/mkdir.md) | 🟡 Creates dirs |
| `rmdir` | [rmdir.md](posix/rmdir.md) | 🟡 Removes dirs |
| `ln` | [ln.md](posix/ln.md) | 🟡 Creates links |
| `ls` | [ls.md](posix/ls.md) | 🟢 Read-only |
| `chmod` | — | 🟡 Changes permissions |

### Tier 4: Text processing (complex algorithms)

| Utility | Notes |
|---------|-------|
| `grep` | Pattern matching. Hardest utility to verify. |
| `sed` | Stream editor. Very complex. |
| `diff` | Myer's algorithm. Complex but provable. |
| `find` | Directory traversal + predicate logic. |
| `xargs` | Argument batching + process spawning. |
| `test` / `[` | Shell conditionals. Must match bash behavior. |

### Tier 5: Nice to have (not needed for self-replication)

| Utility | Notes |
|---------|-------|
| `du` | Disk usage. Syscall-heavy. |
| `dd` | Block copy/conversion. Performance-critical. |
| `patch` | Ed script interpreter. |
| `expr` | Arithmetic expression evaluator. |
| `yes` | Trivial but useful. |
| `uname` | `uname` syscall wrapper. Trivial. |
| `touch` | `utimensat` wrapper. Trivial. |

## Progress

```
Self-replication goal: 28 utilities
Implemented:             2  (cat, true)
Hardcoded in dispatcher: 1  (false)
Remaining:              25
                          ████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░  7%
```

### Per-utility checklist

For each utility:
- [ ] `lean-exe` target added to lakefile.lean
- [ ] `Logic.lean` — pure function with formal proof (no `sorry`/`admit`)
- [ ] `<Util>.lean` — thin IO wrapper using `Common/IO/Fd` FFI
- [ ] Registered in `Main.lean` dispatcher
- [ ] POSIX spec consulted (`wiki/posix/<util>.md`)
- [ ] ToyBox test adapted (`tests/sandbox/<util>.test`)
- [ ] `tests/sandbox/run-sandbox-tests.sh <util>` — all pass
- [ ] Symlink in `scripts/install-symlinks.sh`

> Sources: POSIX.1-2017 (wiki/posix/), ToyBox roadmap (wiki/toybox-roadmap.md).
> License: 0BSD. Do NOT read GPL source.
