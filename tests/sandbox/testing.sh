#!/bin/bash
# testing.sh — Adapted from ToyBox's runtest.sh (0BSD, Rob Landley)
# Adds: bwrap sandbox isolation, FS-state diffing, JSON output mode
#
# License: 0BSD (inherited from ToyBox)
#
# Core function: testing "name" "command" "expected_stdout" "infile" "stdin"
# Sets $C to the binary under test. With TEST_HOST=1, $C = host binary.

export FAILCOUNT=0 SKIP=0 TOTAL=0 PASSED=0
export RESULTS_JSON="[]"

# ─── bwrap sandbox ─────────────────────────────────────────────────────────────

# Run a command inside a bwrap sandbox with a tmpfs root.
# Usage: sandbox_run <fixture_dir> <size_bytes> <working_subdir> -- <cmd...>
sandbox_run() {
  local fixture="$1" size="$2" workdir="$3"; shift 3
  [ "$1" = "--" ] && shift

  bwrap \
    --unshare-all \
    --die-with-parent \
    --dev /dev \
    --proc /proc \
    --ro-bind /bin /bin \
    --ro-bind /lib /lib \
    --ro-bind /lib64 /lib64 2>/dev/null \
    --size "$size" --tmpfs /sandbox \
    ${fixture:+--ro-bind "$fixture" /fixture} \
    -- /bin/sh -c "cd /sandbox/$workdir 2>/dev/null; cp -a /fixture/. . 2>/dev/null; exec \"\$@\"" -- "$@"
}

# Snapshot filesystem state: file tree + sha256 of each file.
# Output: sorted "path|sha256|size|mode" lines.
fs_snapshot() {
  local dir="${1:-.}"
  ( cd "$dir" 2>/dev/null && find . -type f -o -type l | sort | while read -r f; do
    local hash size mode
    hash=$(sha256sum "$f" 2>/dev/null | cut -d' ' -f1)
    size=$(stat -c '%s' "$f" 2>/dev/null || echo 0)
    mode=$(stat -c '%a' "$f" 2>/dev/null || echo 0)
    printf '%s|%s|%s|%s\n' "$f" "$hash" "$size" "$mode"
  done; find . -type d | sort | while read -r d; do
    printf 'DIR|%s|||\n' "$d"
  done )
}

# ─── JSON result accumulator ───────────────────────────────────────────────────

add_result() {
  local utility="$1" name="$2" passed="$3" diff="$4" fs_mismatch="$5"
  # Escape diff for JSON
  local escaped_diff
  escaped_diff=$(printf '%s' "$diff" | jq -Rs '.')
  local entry
  entry=$(jq -n \
    --arg utility "$utility" \
    --arg name "$name" \
    --argjson passed "$passed" \
    --argjson diff "$escaped_diff" \
    --argjson fs_mismatch "$fs_mismatch" \
    '{utility: $utility, name: $name, passed: $passed, diff: $diff, fsMismatch: $fs_mismatch}')
  RESULTS_JSON=$(printf '%s' "$RESULTS_JSON" | jq -c --argjson entry "$entry" '. + [$entry]')
}

# ─── Core test function (adapted from ToyBox) ──────────────────────────────────

# testing "name" "command" "expected_stdout" "infile_content" "stdin_content"
# If DESTRUCTIVE=1, runs inside a bwrap sandbox and diffs FS state.
#
# In test commands:
#   $C     = path to our binary (for file references like: cat "$C")
#   $UTIL  = full utility invocation — for host: /bin/<util>, for ours: <OUR_BINARY> <util>
#
# The testing function substitutes $UTIL differently for host vs ours runs,
# enabling true differential testing.
testing() {
  [ $# -ne 5 ] && { echo "Test has wrong args ($# $*)" >&2; return; }

  local tname="$1" cmd="$2" expected="$3" infile="$4" stdin="$5"
  local NAME="${tname:-$cmd}"

  TOTAL=$((TOTAL + 1))

  if [ "$SKIP" -gt 0 ]; then
    SKIP=$((SKIP - 1))
    return 0
  fi

  local tmpdir
  tmpdir=$(mktemp -d)

  # Write expected output
  printf '%b' "$expected" > "$tmpdir/expected"

  # Write input file if provided
  [ -n "$infile" ] && printf '%b' "$infile" > "$tmpdir/input" || rm -f "$tmpdir/input"

  # Snapshot FS before (for destructive tests)
  local fs_before="" fs_after_host="" fs_after_ours="" fs_diff=""

  if [ "${DESTRUCTIVE:-0}" = "1" ]; then
    # Create a fixture from the input file
    local fixture="$tmpdir/fixture"
    mkdir -p "$fixture"
    [ -n "$infile" ] && cp "$tmpdir/input" "$fixture/input"
    fs_before=$(cd "$fixture" && fs_snapshot .)
  fi

  # Prepare host and ours versions of the command
  # $UTIL → host: system binary (/bin/<util>), ours: our binary invocation
  local host_cmd="${cmd//\$UTIL/$HOST_UTIL}"
  local ours_cmd="${cmd//\$UTIL/$OUR_UTIL}"
  # Also substitute $C with the binary path (same for both)
  host_cmd="${host_cmd//\$C/$C}"
  ours_cmd="${ours_cmd//\$C/$C}"

  # Run against host binary (baseline)
  local host_out host_exit
  if [ "${DESTRUCTIVE:-0}" = "1" ]; then
    host_out=$(sandbox_run "$fixture" 1048576 test -- sh -c "echo -ne '$stdin' | $host_cmd" 2>&1)
  else
    host_out=$(printf '%b' "$stdin" | eval "$host_cmd" 2>&1)
  fi
  host_exit=$?
  printf '%s' "$host_out" > "$tmpdir/host_out"

  # Run against our binary
  local ours_out ours_exit
  if [ "${DESTRUCTIVE:-0}" = "1" ]; then
    ours_out=$(sandbox_run "$fixture" 1048576 test -- sh -c "echo -ne '$stdin' | $ours_cmd" 2>&1)
  else
    ours_out=$(printf '%b' "$stdin" | eval "$ours_cmd" 2>&1)
  fi
  ours_exit=$?
  printf '%s' "$ours_out" > "$tmpdir/ours_out"

  # Diff stdout
  local out_diff
  out_diff=$(diff -au "$tmpdir/host_out" "$tmpdir/ours_out" 2>&1)

  # Diff exit codes
  local exit_mismatch=""
  [ "$host_exit" != "$ours_exit" ] && exit_mismatch="exit: host=$host_exit ours=$ours_exit"

  # FS state diff (destructive only)
  local fs_mismatch="false"
  if [ "${DESTRUCTIVE:-0}" = "1" ]; then
    fs_after_host=$(sandbox_run "$fixture" 1048576 test -- sh -c "echo -ne '$stdin' | $host_cmd >/dev/null 2>&1; fs_snapshot ." 2>/dev/null || echo "")
    fs_after_ours=$(sandbox_run "$fixture" 1048576 test -- sh -c "echo -ne '$stdin' | $ours_cmd >/dev/null 2>&1; fs_snapshot ." 2>/dev/null || echo "")
    if [ "$fs_after_host" != "$fs_after_ours" ]; then
      fs_mismatch="true"
      fs_diff=$(diff <(printf '%s\n' "$fs_after_host") <(printf '%s\n' "$fs_after_ours") 2>&1 | head -20)
    fi
  fi

  # Determine pass/fail
  local all_diff=""
  [ -n "$out_diff" ] && all_diff="STDOUT DIFF:\n$out_diff\n"
  [ -n "$exit_mismatch" ] && all_diff="${all_diff}EXIT MISMATCH:\n$exit_mismatch\n"
  [ "$fs_mismatch" = "true" ] && all_diff="${all_diff}FS STATE DIFF:\n$fs_diff\n"

  if [ -z "$all_diff" ]; then
    PASSED=$((PASSED + 1))
    add_result "$CMDNAME" "$NAME" "true" "" "false"
  else
    FAILCOUNT=$((FAILCOUNT + 1))
    add_result "$CMDNAME" "$NAME" "false" "$all_diff" "$fs_mismatch"
    printf 'FAIL: %s %s\n' "$CMDNAME" "$NAME" >&2
    printf '%b\n' "$all_diff" >&2
  fi

  rm -rf "$tmpdir"
  return 0
}

# testcmd: like testing but uses $UTIL instead of bare command
# Automatically prepends "$UTIL" to the command.
testcmd() {
  [ $# -ne 5 ] && { echo "testcmd has wrong args ($# $*)" >&2; return; }
  testing "$1" "\"\$UTIL\" $2" "$3" "$4" "$5"
}

# Skip next N tests
optional() { SKIP=99999; }
skipnot() { eval "$@" >/dev/null 2>&1 || { ((++SKIP)); return 1; }; }
toyonly() { [ -n "$TEST_HOST" ] && ((++SKIP)); "$@"; }

# ─── JSON output ───────────────────────────────────────────────────────────────

print_json() {
  printf '%s' "$RESULTS_JSON" | jq '{summary: ((map(select(.passed)) | length | tostring) + "/" + (length | tostring) + " tests passed"), allPassed: ((map(select(.passed | not)) | length) == 0), totalTests: length, passed: (map(select(.passed)) | length), failed: (map(select(.passed | not)) | length), failures: map(select(.passed | not))}'
}
