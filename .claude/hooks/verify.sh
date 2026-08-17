#!/usr/bin/env bash
#
# Stop hook: refuse to let a session end with a failing harness.
#
# Exit 0 lets the session end. Exit 2 blocks it, and whatever this script
# writes to stderr becomes the agent's instructions for what to fix.
#
# WHY THIS DOES NOT RUN THE WHOLE SUITE
#
# The full suite is about 109 seconds, and that is its floor: its cost is
# dominated by two gg_partial_varpro tests whose expense IS the function under
# test (partialpro), not setup, so no amount of fixture work makes it cheap.
# See the Phase 3 profiling in AGENTS.md. Lint plus a full suite would put
# about 126 seconds on the end of every session, and a gate that expensive gets
# switched off within a week, which is worse than no gate.
#
# So: lint always (17 s, catches the cheap failures), then only the test files
# that correspond to what actually changed. CI and the pre-PR
# `R CMD check --as-cran` remain the full-coverage gates. This hook is the fast
# one, and it is allowed to miss a cross-file breakage that CI will catch.
set -uo pipefail

payload=$(cat)

# Mandatory loop guard. Without it, a failure the agent cannot fix traps the
# session in a stop / block / stop cycle forever.
if [ "$(printf '%s' "$payload" | jq -r '.stop_hook_active // false')" = "true" ]; then
  exit 0
fi

cd "${CLAUDE_PROJECT_DIR:-$PWD}" || exit 0

# Only source and test changes are worth verifying. A session that read files,
# edited a vignette or updated docs ends immediately.
changed=$(
  {
    git diff  --name-only HEAD -- R/ tests/ 2>/dev/null
    git ls-files --others --exclude-standard -- R/ tests/ 2>/dev/null
  } | sort -u
)
[ -z "$changed" ] && exit 0

# NOT_CRAN keeps the 34 skip_on_cran() tests live: without it they report SS
# and a run that skipped everything looks like a run that passed.
#
# VDIFFR_RUN_TESTS is the one that destroys work. Unset, testthat treats every
# vdiffr baseline as unused and deletes all 49 of them. .Renviron already
# defaults it to true, but this hook must not depend on that: R --vanilla
# ignores .Renviron.
export NOT_CRAN=true
export VDIFFR_RUN_TESTS=true

fail () {
  printf 'Do not stop yet. The verification harness is failing.\n\n%s\n' "$1" >&2
  exit 2
}

# ---- 1. lint -----------------------------------------------------------------
if ! lint_out=$(Rscript -e 'l <- lintr::lint_package(); if (length(l)) { print(l); quit(status = 1) }' 2>&1); then
  fail "lintr::lint_package() reported lints. Fix these, then re-run:

$lint_out

Note that cyclocomp_linter caps complexity at 20. When a function grows one
branch too many the fix is to extract a helper, not to raise the cap."
fi

# ---- 2. targeted tests -------------------------------------------------------
# Map each changed file to a testthat filter token. Test files map to
# themselves; R/ files map to their gg_* family, so R/plot.gg_varpro.R and
# R/gg_varpro.R both select test_gg_varpro.R.
#
# Deliberately a function rather than a `case` inside the $( ) below. A case
# pattern ends in ')', and inside a command substitution bash reads that ')' as
# the closing paren of the $( , which is a parse error several lines later at
# the first ';;'. The Phase 5f tests caught this, having first "passed" three
# cases for the wrong reason: a crashing hook also exits non-zero.
token_for () {
  local f="$1"
  case "$f" in
    tests/testthat/test_*.R)
      basename "$f" .R | sed 's/^test_//' ;;
    R/*.R)
      basename "$f" .R | sed -e 's/^\.//' -e 's/^plot\.//' -e 's/^autoplot\.//' \
                             -e 's/^print\.//' -e 's/^summary\.//' ;;
  esac
}

tokens=$(
  printf '%s\n' "$changed" | while read -r f; do
    token_for "$f"
  done | sed 's/[^A-Za-z0-9_.]//g' | grep -v '^$' | sort -u
)

if [ -z "$tokens" ]; then
  exit 0
fi

filter=$(printf '%s' "$tokens" | paste -sd'|' -)

test_out=$(Rscript -e "
  suppressMessages(devtools::load_all(quiet = TRUE))
  res <- testthat::test_local(filter = '$filter', reporter = 'summary',
                              stop_on_failure = FALSE)
  df <- as.data.frame(res)
  bad <- sum(df\$failed) + sum(df\$error)
  cat('\nFILTER: $filter\n')
  cat('FILES:', length(unique(df\$file)), ' FAILED:', bad, '\n')
  if (bad > 0) quit(status = 1)
" 2>&1) || fail "Tests failed for the files you changed.

$test_out

Run the full suite before opening a PR:
  NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::test()'"

# ---- 3. the snapshot guard ---------------------------------------------------
# The env vars above should make this impossible, but this repo has lost its
# vdiffr baselines repeatedly and a deletion is far cheaper to catch here than
# after it is committed.
if git status --porcelain -- tests/testthat/_snaps | grep -q '^.D\|^D'; then
  fail "Deleted vdiffr baselines are present in the working tree:

$(git status --porcelain -- tests/testthat/_snaps)

That is the signature of a suite run without VDIFFR_RUN_TESTS=true. Restore
them with:
  git checkout -- tests/testthat/_snaps/

Do NOT commit the deletions. If you meant to retire a baseline, say so
explicitly rather than letting a prune through."
fi

exit 0
