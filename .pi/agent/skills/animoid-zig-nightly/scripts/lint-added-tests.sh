#!/usr/bin/env bash
set -euo pipefail

repo=${1:?usage: lint-added-tests.sh REPO [BASE]}
base=${2:-main}
cd "$repo"

# Inspect only added lines in test sources. This is a guardrail, not semantic proof.
diff=$(git diff --unified=0 "$base"...HEAD -- '*.zig' '*.kt' '*Test.kt' '*test.zig' || true)
if [[ -z "$diff" ]]; then
  echo "no committed test diff against $base"
  exit 0
fi

fail=0
check() {
  local pattern=$1
  local message=$2
  if grep -nE '^\+[^+].*' <<<"$diff" | grep -E "$pattern"; then
    echo "ERROR: $message" >&2
    fail=1
  fi
}

check 'expect\(true\)|assertTrue\(true\)' 'unconditional assertion'
check 'does not crash|no crash|smoke only' 'crash-only test name/claim'
check 'expect\([^)]*(>|>=)[^)]*0|assert[^ (]*\([^)]*(>|>=)[^)]*0' 'non-zero/threshold assertion where exact proof is required'
check 'hash[^\n]*(!=|==)[[:space:]]*0' 'metadata/hash sentinel assertion'
check 'currentFormula|fixedFormula|expectedFormula[[:space:]]*=' 'local formula oracle; call production symbol instead'

if (( fail )); then
  echo "added-test lint FAILED" >&2
  exit 1
fi
echo "added-test lint passed"
