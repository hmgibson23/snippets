#!/usr/bin/env bash
set -euo pipefail

if (( $# < 3 )); then
  echo "usage: $0 <repo> <zig-test-filter> <expected-test-name-substring> [...]" >&2
  exit 64
fi

repo=$1
filter=$2
shift 2
zig_bin=${ZIG_BIN:-/opt/homebrew/bin/zig}
output=$(mktemp)
trap 'rm -f "$output"' EXIT

cd "$repo"
set +e
"$zig_bin" test src/run_tests.zig -O ReleaseFast --test-filter "$filter" 2>&1 | tee "$output"
status=${PIPESTATUS[0]}
set -e
if (( status != 0 )); then
  echo "focused Zig command failed: filter=$filter exit=$status" >&2
  exit "$status"
fi

missing=0
for expected in "$@"; do
  if ! grep -F -- "$expected" "$output" >/dev/null; then
    echo "MISSING EXPECTED TEST IN OUTPUT: $expected" >&2
    missing=1
  fi
done

if (( missing != 0 )); then
  echo "Wrapper/import tests such as run_tests.test_0 do not satisfy expected test names." >&2
  exit 65
fi

matched=$(grep -c 'test\..*\.\.\.OK' "$output" || true)
if (( matched == 0 )); then
  echo "No named Zig test execution lines found." >&2
  exit 66
fi

echo "Focused Zig gate verified filter '$filter' and $# expected test name(s)."
