#!/bin/bash
# run-sandbox-tests.sh — Run differential sandbox tests for lentils.
#
# Usage:
#   ./run-sandbox-tests.sh [--json] [utility...]
#
# Without arguments, runs all *.test files in tests/sandbox/.
# With --json, outputs machine-readable JSON (for Smithers).
#
# Requires: bwrap, /bin/<utility> (host coreutils for differential testing).
# Set OUR_BINARY to the path of our coreutils binary (default: .lake/build/bin/coreutils).

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
OUR_BINARY="${OUR_BINARY:-$PROJECT_ROOT/.lake/build/bin/coreutils}"
# Resolve to absolute path so wrapper scripts work from any CWD
OUR_BINARY="$(cd "$PROJECT_ROOT" && realpath "$OUR_BINARY" 2>/dev/null || echo "$OUR_BINARY")"
[ -x "$OUR_BINARY" ] || OUR_BINARY="$PROJECT_ROOT/.lake/build/bin/coreutils"

JSON_MODE=0
UTILS=()

while [ $# -gt 0 ]; do
  case "$1" in
    --json) JSON_MODE=1; shift ;;
    --our-binary) OUR_BINARY="$2"; shift 2 ;;
    *) UTILS+=("$1"); shift ;;
  esac
done

# Check our binary exists
if [ ! -x "$OUR_BINARY" ]; then
  if [ "$JSON_MODE" = "1" ]; then
    printf '{"summary":"Binary not found at %s","allPassed":false,"totalTests":0,"passed":0,"failed":0,"failures":[]}' "$OUR_BINARY"
  else
    echo "ERROR: Binary not found at $OUR_BINARY" >&2
    echo "Build first with: lake build" >&2
  fi
  exit 1
fi

# Check bwrap exists
if ! command -v bwrap >/dev/null 2>&1; then
  echo "ERROR: bwrap is required for sandbox testing" >&2
  exit 1
fi

export OUR_BINARY

# Source the testing framework
. "$SCRIPT_DIR/testing.sh"

# If no utilities specified, run all
if [ ${#UTILS[@]} -eq 0 ]; then
  for f in "$SCRIPT_DIR"/*.test; do
    [ -x "$f" ] || [ -f "$f" ] && UTILS+=("$(basename "$f" .test)")
  done
fi

FAILTOTAL=0

for util in "${UTILS[@]}"; do
  testfile="$SCRIPT_DIR/$util.test"
  if [ ! -f "$testfile" ]; then
    echo "SKIP: no test file for $util" >&2
    continue
  fi

  export CMDNAME="$util"
  export C="$OUR_BINARY"
  # Create a wrapper script for our binary so $UTIL expands to a single path.
  # This avoids word-splitting issues with "$OUR_BINARY $util" inside eval.
  export TESTDIR="$(mktemp -d)"
  mkdir -p "$TESTDIR/testdir"
  OUR_WRAPPER="$TESTDIR/our_$util"
  cat > "$OUR_WRAPPER" << WRAPEOF
#!/bin/sh
exec "$OUR_BINARY" "$util" "\$@"
WRAPEOF
  chmod +x "$OUR_WRAPPER"
  # $UTIL is substituted differently for host vs ours runs
  export HOST_UTIL="/bin/$util"
  export OUR_UTIL="$OUR_WRAPPER"
  cd "$TESTDIR"

  # Reset per-utility counters before running test file
  FAILCOUNT=0
  SKIP=0

  # Run the test file in current shell so variables update
  source "$testfile" 2>/dev/null || true
  FAILTOTAL=$((FAILTOTAL + FAILCOUNT))

  cd /
  rm -rf "$TESTDIR"
done

if [ "$JSON_MODE" = "1" ]; then
  print_json
else
  echo ""
  echo "Results: $PASSED/$TOTAL passed, $FAILCOUNT failed"
fi

[ $FAILCOUNT -eq 0 ]
