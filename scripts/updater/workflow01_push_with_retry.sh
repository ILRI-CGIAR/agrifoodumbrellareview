#!/usr/bin/env bash
# Replay an already-created local commit on top of remote head; never force-push.
set -euo pipefail
branch="${1:?branch required}"
for attempt in 1 2 3 4 5; do
  git fetch origin "$branch"
  git rebase "origin/$branch"
  if git push origin "HEAD:$branch"; then
    echo "PASS: registered changes on $branch (attempt $attempt)"
    exit 0
  fi
  echo "WARN: concurrent push to $branch; retrying with freshly fetched head ($attempt/5)" >&2
  sleep "$((attempt*2))"
done
echo "ERROR: could not publish after five retries" >&2
exit 1
