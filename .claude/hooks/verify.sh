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
#
# Reading it needs jq, so a missing or failing jq has to mean "let the session
# end", never "carry on without the guard". Carrying on is precisely the
# endless loop this guard exists to prevent: the hook would block, the retry
# would arrive with stop_hook_active true, and we still could not read it.
# Fail open, and treat unparseable input the same way.
if ! command -v jq >/dev/null 2>&1; then
  exit 0
fi
stop_active=$(printf '%s' "$payload" | jq -r '.stop_hook_active // false' 2>/dev/null) || exit 0
[ "$stop_active" = "true" ] && exit 0

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
      # Strip a method prefix, then truncate at the first remaining dot.
      # R/plot.gg_vimp.R      -> gg_vimp
      # R/surv_partial.rfsrc.R-> surv_partial   (matches test_surv_partial.R)
      # R/ggrandomforests.news.R -> ggrandomforests (matches the _news test)
      # Without the truncation those last two produce tokens that match no
      # test file at all.
      basename "$f" .R | sed -e 's/^\.//' -e 's/^plot\.//' -e 's/^autoplot\.//' \
                             -e 's/^print\.//' -e 's/^summary\.//' | cut -d. -f1 ;;
  esac
}

tokens=$(
  printf '%s\n' "$changed" | while read -r f; do
    token_for "$f"
  done | sed 's/[^A-Za-z0-9_]//g' | grep -v '^$' | sort -u
)

# Which test files do those tokens actually select? Eight files in R/
# (calc_roc.R, print_methods.R, utils.R and friends) have no same-named test
# and are covered indirectly, so "no match" is a normal case, not an edge one.
#
# Running zero tests and exiting 0 would be the worst possible outcome: the
# hook would report success having verified nothing. So when targeting fails,
# fall back to the whole suite. Cheap when we can target, correct when we
# cannot.
matched=""
if [ -n "$tokens" ]; then
  matched=$(
    printf '%s\n' "$tokens" | while read -r t; do
      ls tests/testthat/test_*.R 2>/dev/null | grep -- "$t" || true
    done | sort -u
  )
fi

# Cross-cutting test files, always run when anything changed.
#
# These pin invariants across the whole package rather than testing one
# extractor, so they are named after the property they check and NO per-file
# token will ever select them. Leaving them out made the gate miss the thing it
# exists for: editing R/gg_rfsrc.R selected only test_gg_rfsrc.R, while the
# cross-check that catches a wrong value (predicted.oob swapped for the in-bag
# predicted) lives in test_extractor_contracts.R. The hook passed a forest
# extractor that had been made to report in-bag predictions.
#
# Both halves of the harness worked in isolation and did not compose. Only an
# end-to-end run against merged main showed it.
#
# All six together cost about 3.2 seconds, so this is close to free.
always='extractor_contracts|autoplot_equivalence|determinism|plot_conventions|default_dispatch|namespace_hygiene'

if [ -n "$matched" ]; then
  filter=$(printf '%s' "$tokens" | paste -sd'|' -)
  filter="$filter|$always"
  # No quote characters in scope: it is interpolated into an R string literal
  # inside a double-quoted bash string, and a stray ' ends that literal early.
  scope="filter $filter"
else
  filter=""
  scope="the FULL suite (nothing under R/ mapped to a test file)"
fi

test_out=$(Rscript -e "
  suppressMessages(devtools::load_all(quiet = TRUE))
  f   <- '$filter'
  res <- testthat::test_local(filter = if (nzchar(f)) f else NULL,
                              reporter = 'summary', stop_on_failure = FALSE)
  df  <- as.data.frame(res)
  bad <- sum(df\$failed) + sum(df\$error)
  cat('\nSCOPE: $scope\n')
  cat('FILES:', length(unique(df\$file)), ' FAILED:', bad, '\n')
  # Running nothing is not a pass. If the scope selected no files at all,
  # something is wrong with the mapping and the run proved nothing.
  if (length(unique(df\$file)) == 0L) quit(status = 1)
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
