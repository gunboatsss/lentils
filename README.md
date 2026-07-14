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

## Utilities — 87 applets

### System Info & Identity (15)

| Utility | Description | Notes |
|---|---|---|
| **arch** | print machine architecture | reads `/proc/sys/kernel/arch` |
| **groups** | print group memberships | C FFI (`getpwuid`, `getgrgid`) |
| **hostid** | print numeric host identifier | reads `/proc/sys/kernel/hostid` |
| **id** | print user and group identity | C FFI (`getpwuid`, `getgrgid`) |
| **logname** | print user's login name | reads `/proc/self/loginuid` → `getpwuid_r` |
| **mesg** | check/set terminal write access | — |
| **nproc** | print number of processing units | reads `/proc/cpuinfo` |
| **pinky** | lightweight finger | not yet |
| **printenv** | print environment variables | C FFI (`environ`) |
| **tty** | print terminal file name | reads `/proc/self/fd/0` |
| **uname** | print system information | reads `/proc/sys/kernel/*` |
| **uptime** | print system uptime | reads `/proc/uptime` |
| **users** | print logged-in user names | C FFI (`getutxent`) |
| **who** | list logged-in users | — |
| **whoami** | print effective user name | reads `/proc/self/status` |

### File Operations (18)

| Utility | Description | Notes |
|---|---|---|
| **basename** | strip directory and suffix from pathname | — |
| **cat** | concatenate files to stdout | — |
| **chmod** | change file mode bits | numeric & symbolic modes |
| **chown** | change file owner and group | not yet |
| **chgrp** | change group ownership | not yet |
| **cp** | copy files and directories | `-r`, `-f`, `-v` |
| **dd** | convert and copy with block size options | — |
| **dirname** | strip last component from file name | — |
| **install** | copy files and set attributes | — |
| **ln** | make links between files | hard & symbolic |
| **ls** | list directory contents | basic listing |
| **mkdir** | make directories | `-p` |
| **mkfifo** | make named pipes (FIFOs) | not yet |
| **mknod** | make block/character special files | not yet |
| **mv** | move (rename) files | — |
| **readlink** | print target of a symbolic link | — |
| **realpath** | print canonical absolute path | — |
| **rm** | remove files or directories | `-r`, `-f` |
| **rmdir** | remove empty directories | — |
| **touch** | change file timestamps | — |
| **truncate** | shrink or extend a file to a specified size | — |
| **unlink** | call the unlink() syscall | not yet |

### Text Processing (24)

| Utility | Description | Notes |
|---|---|---|
| **cksum** | POSIX CRC checksum | — |
| **comm** | compare two sorted files | `-1`, `-2`, `-3` |
| **csplit** | split file by context lines | not yet |
| **cut** | extract sections from each line | `-c`, `-f`, `-d`, `-s`; `-b` not yet |
| **expand** | convert tabs to spaces | — |
| **fmt** | reformat paragraph text | — |
| **fold** | wrap lines at a specified width | `-w`, `-s` |
| **grep** | print lines matching a pattern | `-v`, `-e`, `-i`, `-c`, `-q`, `-n`, `-l`, `-w` |
| **head** | output the first part of files | `-n` |
| **join** | join lines on a common field | — |
| **nl** | number lines of input | — |
| **od** | dump files in octal format | basic octal dump |
| **paste** | merge lines of files | `-d`, `-s` |
| **pr** | paginate text into pages | — |
| **printf** | write formatted output | `%s`, `%d`, `%%`, `\n`, `\t`, `\\` |
| **ptx** | produce permuted index | — |
| **shuf** | shuffle lines of input | uses `IO.rand` |
| **sort** | sort lines of text files | `-r`, `-n`, `-t`, `-k`, `-u` |
| **split** | split input into files by size/line count | — |
| **sum** | BSD checksum and block counts | — |
| **tac** | reverse concatenate (reverse cat) | not yet |
| **tail** | output the last part of files | `-n` |
| **tee** | read stdin and write to stdout and files | `-a` |
| **tr** | translate or delete characters | `-d`, `-s`, `-c`, `-C`; ranges `a-z` |
| **tsort** | topological sort | Kahn's algorithm |
| **unexpand** | convert spaces to tabs | — |
| **uniq** | report or omit repeated lines | `-u`, `-d` |
| **wc** | word, line, and byte count | `-l`, `-w`, `-c` |

### Hash & Checksum (7)

| Utility | Description | Notes |
|---|---|---|
| **b2sum** | compute BLAKE2b hash | — |
| **md5sum** | compute MD5 hash | — |
| **sha1sum** | compute SHA-1 hash | — |
| **sha224sum** | compute SHA-224 hash | not yet |
| **sha256sum** | compute SHA-256 hash | — |
| **sha384sum** | compute SHA-384 hash | not yet |
| **sha512sum** | compute SHA-512 hash | — |

### Encoding (3)

| Utility | Description | Notes |
|---|---|---|
| **base32** | base32 encode/decode | not yet |
| **base64** | base64 encode/decode | — |
| **basenc** | generic base encoding | not yet |

### Math & Conversion (5)

| Utility | Description | Notes |
|---|---|---|
| **expr** | evaluate integer/string expressions | — |
| **factor** | factor integers into primes | — |
| **numfmt** | convert numbers to/from human-readable form | — |
| **seq** | print sequence of numbers | custom float parser |
| **test** | check file types and compare values | string/int/file ops; `[ ]` alias |

### Process & Execution (9)

| Utility | Description | Notes |
|---|---|---|
| **env** | run a command with modified environment | — |
| **kill** | send a signal to a process | — |
| **nice** | run with modified scheduling priority | — |
| **nohup** | run a command immune to hangups | — |
| **sleep** | suspend execution for an interval | — |
| **stdbuf** | buffer standard I/O | not yet |
| **timeout** | run a command with a time limit | — |
| **stty** | set terminal characteristics | not yet |
| **sync** | synchronize cached writes to disk | not yet |

### Boolean & Misc (10)

| Utility | Description | Notes |
|---|---|---|
| **echo** | write arguments to stdout | — |
| **false** | exit with status 1 | — |
| **link** | call the link() syscall | not yet |
| **mktemp** | create temporary files/dirs | not yet |
| **more** | page through text files | basic pager |
| **pathchk** | check pathname validity | — |
| **pwd** | print working directory | — |
| **shred** | securely delete files | not yet |
| **true** | exit with status 0 | — |
| **yes** | repeat a string until killed | — |

### Calendars & Date (2)

| Utility | Description | Notes |
|---|---|---|
| **cal** | display a calendar | — |
| **date** | print or set the system date and time | — |

### Disk & Filesystem (3)

| Utility | Description | Notes |
|---|---|---|
| **df** | report file system disk space usage | — |
| **du** | estimate file space usage | — |
| **stat** | display file or file system status | — |

### Not Yet Implemented (from GNU coreutils)

| Utility | Description |
|---|---|
| base32 | base32 encode/decode |
| basenc | generic base encoding |
| chcon | change SELinux security context |
| chgrp | change group ownership |
| chown | change file owner and group |
| csplit | split file by context lines |
| dir | list directory contents (`ls -C` alias) |
| dircolors | color setup for `ls` |
| link | call link() syscall |
| mkfifo | make named pipes |
| mknod | make block/character special files |
| mktemp | create temporary files |
| pinky | lightweight finger |
| runcon | run with SELinux context |
| sha224sum | compute SHA-224 digest |
| sha384sum | compute SHA-384 digest |
| shred | securely delete files |
| stdbuf | buffer standard I/O |
| stty | set terminal characteristics |
| sync | synchronize cached writes |
| tac | reverse concatenate |
| unlink | call unlink() syscall |
| vdir | list directory contents (`ls -l` alias) |

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
