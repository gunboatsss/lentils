# Provenance

This file documents the origin of non-trivial implementation decisions
and external references used in lentils.

## Core implementation source

All utilities are implemented as clean-room reimplementations from
public specifications:

- **POSIX.1-2017 (IEEE Std 1003.1-2017) / The Open Group Base Specifications
  Issue 7** — the authoritative specification for each utility's behavior,
  exit codes, and error messages. Downloaded copies are in `wiki/posix/<util>.md`
  for offline reference during implementation.
- **Linux man pages** (man7.org, die.net) — for system call signatures
  (open(2), close(2), read(2), write(2), strerror(3)).
- **Linux man pages** (man7.org, die.net) — for system call signatures
  (open(2), close(2), read(2), write(2), strerror(3)).
- **Lean 4 language reference** and standard library documentation —
  for the ByteArray API, IO monad, and FFI (@[extern]) mechanism.

## Explicitly NOT consulted

The following implementations were **not** read or referenced while
writing the Lean implementation of each utility:

- **GNU coreutils** (GPLv3+) — source code was intentionally avoided to
  prevent derivative-work risk. Only the POSIX specification and man pages
  were used to determine correct behavior.
- **BusyBox** (GPLv2) — source code was intentionally avoided for the
  same reason.

## Permissive references

- **ToyBox** (0BSD, Rob Landley) — the testing framework
  (`tests/sandbox/testing.sh`, `tests/sandbox/cat.test`,
  `tests/sandbox/run-sandbox-tests.sh`) is adapted from ToyBox's
  `scripts/runtest.sh`. ToyBox is 0BSD licensed and its testing
  infrastructure is used as a reference for differential test structure.

- **uutils/coreutils** (MIT) — not yet referenced. If uutils code
  is adapted in the future, attribution will be added here and in
  THIRD_PARTY_NOTICES.md.

## Architecture

The single-binary + symlink dispatch pattern is an architectural idea
used by BusyBox, ToyBox, and many other projects. The idea itself is
not copyrightable; our implementation is original Lean code.

## Verification approach

The pure/IO split (Logic.lean for verified pure functions, <Util>.lean
for IO wrappers) is an application of standard functional programming
and formal methods practices. The specific properties to prove per utility
are derived from POSIX behavioral requirements.
