# lentils

A [coreutils](https://en.wikipedia.org/wiki/GNU_Core_Utilities) rewrite in [Lean 4](https://lean-lang.org/), following the toybox multi-call binary pattern. Heavily inspired by [toybox](https://landley.net/toybox/). Shoutout to Rob Landley.

Each utility is implemented with a pure logic layer and an IO wrapper, with differential sandbox tests against the system coreutils.

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
.lake/build/bin/lentils --help    # list all applets
.lake/build/bin/lentils cat --help  # help for specific applet
```

## Utilities — 42 applets

### System Info (13)

| Utility | Description | Notes |
|---|---|---|
| **arch** | print machine architecture | reads `/proc/sys/kernel/arch` |
| **hostid** | print numeric host identifier | reads `/proc/sys/kernel/hostid` |
| **logname** | print user's login name | reads `/proc/self/loginuid` → `getpwuid_r` |
| **nproc** | print number of processing units | reads `/proc/cpuinfo` |
| **printenv** | print environment variables | C FFI (`environ`) |
| **seq** | print sequence of numbers | custom float parser |
| **tty** | print terminal file name | reads `/proc/self/fd/0` |
| **uname** | print system information | reads `/proc/sys/kernel/*` |
| **uptime** | print system uptime | reads `/proc/uptime` |
| **users** | print logged-in user names | C FFI (`getutxent`) |
| **whoami** | print effective user name | reads `/proc/self/status` |
| **groups** | print group memberships | C FFI (`getpwuid`, `getgrgid`) |
| **id** | print user and group identity | C FFI (`getpwuid`, `getgrgid`) |

### Text Processing (17)

| Utility | Description | Notes |
|---|---|---|
| **cat** | concatenate files to stdout | — |
| **cut** | extract sections from each line | `-c`, `-f`, `-d`, `-s`; `-b` not yet |
| **expand** | convert tabs to spaces | — |
| **fold** | wrap lines at a specified width | `-w`, `-s` |
| **grep** | print lines matching a pattern | regex: `. * ^ $ [abc] [^abc] \\`; `-v`, `-e`, `-i`, `-c`, `-q`, `-n`, `-l`, `-w` |
| **head** | output the first part of files | `-n` |
| **join** | join lines on a common field | — |
| **nl** | number lines of input | — |
| **paste** | merge lines of files | `-d`, `-s` |
| **printf** | write formatted output | `%s`, `%d`, `%%`, `\n`, `\t`, `\\` |
| **shuf** | shuffle lines of input | uses `IO.rand` |
| **sort** | sort lines of text files | `-r`, `-n`, `-t`, `-k`, `-u` |
| **tail** | output the last part of files | `-n` |
| **tr** | translate or delete characters | `-d`, `-s`, `-c`, `-C`; ranges `a-z` |
| **tsort** | topological sort | Kahn's algorithm |
| **unexpand** | convert spaces to tabs | — |
| **uniq** | report or omit repeated lines | `-u`, `-d` |
| **wc** | word, line, and byte count | `-l`, `-w`, `-c` |

### Utility (7)

| Utility | Description | Notes |
|---|---|---|
| **basename** | strip directory and suffix from pathname | — |
| **comm** | compare two sorted files | `-1`, `-2`, `-3` |
| **dirname** | strip last component from file name | — |
| **echo** | write arguments to stdout | — |
| **tee** | read stdin and write to stdout and files | `-a` |
| **test** | check file types and compare values | string/int/file ops; `[ ]` alias |
| **yes** | repeat a string until killed | — |

### Boolean/Exit (4)

| Utility | Description |
|---|---|
| **true** | exit with status 0 |
| **false** | exit with status 1 |
| **pwd** | print working directory |
| **sleep** | suspend execution for an interval |

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
  Logic.lean    — pure functions (no IO, no sorry/admit)
  <Utility>.lean — IO wrapper (parse args, read/write files, call Logic)
<Utility>.lean   — re-export module
Main.lean         — multi-call dispatcher (argv[0] → utility)
c/coreutils.c     — POSIX FFI bindings (environ, getpwuid, getgrgid, getutxent)
tests/sandbox/    — differential tests against host coreutils
wiki/posix/       — POSIX spec reference copies
```

## License

0BSD
