#!/usr/bin/env bash
set -euo pipefail

repo=${1:?usage: check-worktree-complete.sh REPO BASE [ALLOWED_UNTRACKED ...] -- COMMITS...}
base=${2:?missing base}
shift 2
allowed=()
required=()
mode=allowed
for arg in "$@"; do
  if [[ "$arg" == "--" ]]; then mode=commits; continue; fi
  if [[ "$mode" == allowed ]]; then allowed+=("$arg"); else required+=("$arg"); fi
done
cd "$repo"

fail=0
if ! git diff --quiet; then echo 'ERROR: unstaged changes remain'; git diff --stat; fail=1; fi
if ! git diff --cached --quiet; then echo 'ERROR: staged changes remain'; git diff --cached --stat; fail=1; fi
while IFS= read -r path; do
  [[ -z "$path" ]] && continue
  ok=0
  for permit in "${allowed[@]}"; do [[ "$path" == "$permit" ]] && ok=1; done
  if (( ! ok )); then echo "ERROR: untracked path not allowed: $path"; fail=1; fi
done < <(git status --short | sed -n 's/^?? //p')

if ! git diff --check; then echo 'ERROR: working-tree whitespace'; fail=1; fi
if ! git diff --check "$base"...HEAD; then echo 'ERROR: base-to-HEAD whitespace'; fail=1; fi
if (( ${#required[@]} > 0 )); then
  for commit in "${required[@]}"; do
    if ! git log --format=%s --all | grep -Fx "$commit" >/dev/null; then echo "ERROR: required commit missing: $commit"; fail=1; fi
  done
fi

git status --short
printf 'HEAD: '; git log -1 --oneline
if (( fail )); then exit 1; fi
echo 'worktree completion gate passed'
