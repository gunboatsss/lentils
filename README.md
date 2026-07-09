# lentils

A [coreutils](https://en.wikipedia.org/wiki/GNU_Core_Utilities) rewrite in [Lean 4](https://lean-lang.org/), following the toybox multi-call binary pattern. Heavily inspired by [toybox](https://landley.net/toybox/). Shoutout Rob Landley.

Each utility is implemented with a pure logic layer (formal proofs) and an IO wrapper, with differential sandbox tests against the system coreutils.

## Setup

```bash
# Prerequisites: Lean 4 (via elan), C compiler, bwrap (for sandbox tests)
curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh | sh
sudo apt install bubblewrap gcc  # or equivalent

# Build
git clone https://github.com/gunboatsss/lentils.git
cd lentils
lake build
```

## Usage

```bash
# Multi-call binary
.lake/build/bin/lentils echo hello
.lake/build/bin/lentils grep pattern file.txt

# Or install wrapper scripts (manual only — requires LENTILS_INSTALL_SAFETY=1)
LENTILS_INSTALL_SAFETY=1 ./scripts/install-symlinks.sh
```

## Utilities

Total: 21 applets (20 implemented + expr planned)

### Tier 1 — Essential (complete)

| Utility | Flags | Proofs | POSIX |
|---|---|---|---|
| **cat** | — | — | ✅ |
| **echo** | — | — | ✅ |
| **true** | — | — | ✅ |
| **false** | — | — | ✅ |
| **pwd** | — | — | ✅ |
| **yes** | — | — | ✅ |
| **sleep** | — | — | ✅ |
| **basename** | — | — | ✅ |
| **dirname** | — | — | ✅ |

### Tier 2 — Streaming (complete)

| Utility | Flags | Proofs | POSIX |
|---|---|---|---|
| **head** | `-n` | — | ✅ |
| **tail** | `-n` | — | ✅ |
| **wc** | `-l`, `-w`, `-c` | — | ✅ |
| **uniq** | `-u`, `-d` | — | ✅ |
| **tee** | `-a` | — | ✅ |
| **printf** | `%s`, `%d`, `%%`, `\n`, `\t`, `\\` | — | ✅ |

### Tier 3 — Text Processing

| Utility | Flags | Proofs | Notes |
|---|---|---|---|
| **cut** | `-c`, `-f`, `-d`, `-s` | `native_decide` examples | `-b` not yet implemented |
| **tr** | translate, `-d`, `-s`, `-c` | 8 theorems | char classes and ranges not yet |
| **sort** | `-r`, `-n`, `-t`, `-k`, `-u` | `native_decide` examples | `-c`, `-m`, `-o` not yet |
| **test** | string/int/logic, `-f`, `-d`, `-e`, `-s`, `[ ]` | 21 theorems | `-r`/`-w`/`-x` approximate |
| **grep** | regex (`. * ^ $ [abc] [^abc] \`), `-v`, `-e`, `-i`, `-c`, `-q`, `-n`, `-l`, `-w` | 43 theorems | `-E`, `-F`, `-r`, `-x` not yet |
| **expr** | — | — | planned |

## Testing

```bash
# Sandbox tests (differential vs system coreutils)
bash tests/sandbox/run-sandbox-tests.sh grep    # 20 tests
bash tests/sandbox/run-sandbox-tests.sh test    # file tests
bash tests/sandbox/run-sandbox-tests.sh cut
bash tests/sandbox/run-sandbox-tests.sh tr
bash tests/sandbox/run-sandbox-tests.sh sort
```

## Architecture

```
Lentils/<Utility>/
  Logic.lean    — pure functions + formal proofs (no IO, no sorry/admit)
  <Utility>.lean — IO wrapper (parse args, read/write files, call Logic)
<Utility>.lean   — re-export module
Main.lean         — multi-call dispatcher (argv[0] → utility)
c/coreutils.c     — POSIX FFI bindings
tests/sandbox/    — differential tests against host coreutils
wiki/posix/       — POSIX spec reference copies
```

## License

0BSD
