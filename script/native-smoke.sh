#!/usr/bin/env bash
set -euo pipefail

ges="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
workspace="$(mktemp -d "${TMPDIR:-/tmp}/geschichte-native-XXXXXX")"
agent="$(mktemp -d "${TMPDIR:-/tmp}/geschichte-agent-XXXXXX")"
native_source="$(mktemp -d "${TMPDIR:-/tmp}/geschichte-git-source-XXXXXX")"
local_clone="$(mktemp -d "${TMPDIR:-/tmp}/geschichte-local-clone-XXXXXX")"
trap 'rm -rf "$workspace" "$agent" "$native_source" "$local_clone"' EXIT

cd "$workspace"
printf 'one\n' > README.md
printf '*.log\n' > .gitignore
printf 'ignored\n' > build.log
"$ges" init -q
test -f .geschichte/repo.edn
test "$("$ges" status --short)" = "?? .gitignore
?? README.md"
"$ges" config user.name Native-Smoke
"$ges" config user.email native@example.test
"$ges" add -A
"$ges" commit -q -m initial
test "$("$ges" log -1 --format=%s)" = "initial"
printf 'changed\n' > README.md
test "$("$ges" diff --name-status)" = $'M\tREADME.md'
"$ges" restore README.md

"$ges" branch feature
printf 'two\n' > README.md
"$ges" add README.md
"$ges" commit -q -m main-change
"$ges" checkout -q feature
test "$(cat README.md)" = "one"
"$ges" checkout -q main
test "$(cat README.md)" = "two"

mkdir -p src/nested
test "$("$ges" -C src/nested rev-parse --show-toplevel)" = "$workspace"

"$ges" worktree add "$agent" main
test "$(cat "$agent/README.md")" = "two"
printf 'agent\n' > "$agent/agent.txt"
"$ges" -C "$agent" add agent.txt
"$ges" -C "$agent" commit -q -m agent-change
"$ges" -C "$agent" workspace publish >/dev/null
test ! -f agent.txt
test -z "$("$ges" status --short)"
"$ges" workspace advance >/dev/null
test "$(cat agent.txt)" = "agent"
"$ges" worktree remove --force "$agent"

git -C "$native_source" init -q -b main
git -C "$native_source" config user.name Native-Smoke
git -C "$native_source" config user.email native@example.test
printf 'from native git\n' > "$native_source/native.txt"
git -C "$native_source" add native.txt
git -C "$native_source" commit -q -m native-import
"$ges" clone -q "$native_source" "$local_clone"
test "$(cat "$local_clone/native.txt")" = "from native git"
test "$("$ges" -C "$local_clone" log -1 --format=%s)" = "native-import"

before="$("$ges" rev-parse HEAD)"
if "$ges" reset --merge HEAD >/dev/null 2>&1; then
  echo "unsupported reset unexpectedly succeeded" >&2
  exit 1
fi
test "$("$ges" rev-parse HEAD)" = "$before"
