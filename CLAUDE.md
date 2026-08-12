# CLAUDE.md

## Before you touch code
Load this repo's codemap first. It lives in the Obsidian vault under `Claude/repomaps/`
and is read via the `read-codemap` skill (`/codemap <repo>`). Orient on the public API
surface and where-things-live *before* editing — do not infer structure from a partial
file read. If the codemap looks stale, say so and offer to refresh it (`/regenerate-codemap`)
rather than working from a guess.

## Four rules
1. **Think before coding.** Don't assume — ask. If the request is ambiguous or a name/path/
   signature is uncertain, surface the confusion instead of running with a guess. One good
   clarifying question beats a confident wrong edit.

2. **Simplicity first.** Write the minimum code that solves the stated problem. No speculative
   abstractions, no "while I'm here" generalizing. For this scientific code, prefer the plain,
   readable form a future reader can follow over the clever one.

3. **Surgical changes.** Touch only what the task requires. Do not refactor, reformat, or
   re-style adjacent code, and do not reorganize imports or rename things that weren't asked
   for. If you spot something worth changing nearby, note it separately — don't fold it in.

4. **Goal-driven execution — define "done" as a passing test.** State what "done" looks like
   before you start, and use tests as the success criterion, not vibes. By language:
   - **R** — `devtools::test()` and `R CMD check` pass, examples run clean.
   - **Python** — the relevant `pytest` / doctests pass.
   - **C** — it compiles clean with warnings on (`-Wall -Wextra`) and its checks pass:
     `make check` / `ctest` for standalone code, or, for C compiled inside an R package,
     `R CMD check` builds the native routines and the R tests that exercise them pass.
     Run under a sanitizer (`-fsanitize=address,undefined`) when touching memory or pointers.
   - **SAS** — the log is clean (no ERROR, no unexpected WARNING or uninitialized-variable
     notes), and output validates against a known reference — `PROC COMPARE` against a
     baseline dataset, or check figures against previously verified results. A run that
     "finished" is not the same as a run that's correct; read the log.

   If there's no test covering the change, add or propose one rather than declaring success
   from inspection.

## Before you push

CI enforces `lint` and `R-CMD-check` on every PR. Run both locally first — a
30-second local `lintr` run beats a full CI cycle to learn the same thing.

1. **`Rscript -e 'lintr::lint_package()'` — must report 0 lints.** The `lint` job
   fails the PR otherwise. `cyclocomp_linter` caps cyclomatic complexity at 20;
   when a function grows one branch too many the fix is to extract a helper, not
   to raise the cap.

2. **`NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::test()'`** — not
   `test_file()` under `load_all()`. `devtools::test()` sets `NOT_CRAN=true`; a
   bare `test_file()` does not, so every `skip_on_cran()` test silently skips and
   the run reports `SS` rather than a result. Reading `SS` as "fine" is how you
   conclude a test passed when it never ran.

   `VDIFFR_RUN_TESTS=true` is not optional here — without it this exact command
   prunes the snapshots. See below.

3. **`R CMD check --as-cran` with the manual** — see the release gate in the
   global CLAUDE.md. It runs with `NOT_CRAN` false, so it does *not* exercise
   `skip_on_cran()` tests. Step 2 is the only thing that does; a green check is
   not evidence that those tests pass.

### vdiffr snapshots

A suite run with `VDIFFR_RUN_TESTS` unset deletes the guarded snapshots as
"unused". Run with `VDIFFR_RUN_TESTS=true` so they register and nothing is
pruned, or restore afterwards. If you regenerate a baseline, do it **last**: a
later full-suite run deletes it, and a blanket
`git checkout -- tests/testthat/_snaps/` to undo that will silently revert your
regeneration along with the pruning.

**Restoring afterwards only recovers the tracked snapshots.** There are 40
tracked SVGs under `tests/testthat/_snaps/snapshots/` and, on a working
checkout, several more that are untracked. `git restore` brings back the 40 and
cannot bring back the rest — git never had them. This happened on 2026-08-06:
one unguarded suite run pruned 49 files, and the 9 untracked ones were only
recovered because a stale copy happened to survive in
`ggRandomForests.Rcheck/00_pkg_src/`. That copy is not a backup and will not
reliably be there.

So the guard is the fix, not the cleanup. Check `git status` before and after
any local suite run, and treat a wave of staged snapshot deletions as a signal
that the env var was missing — not as a change to commit.

**Two protections now sit under that advice, because remembering an env var has
failed repeatedly.**

1. **`.Renviron` inverts the default.** It sets
   `VDIFFR_RUN_TESTS=${VDIFFR_RUN_TESTS-true}`, so an unset variable — the
   state of every fresh shell, script and agent session, and the cause of every
   prune so far — now reads as `true`. An explicit value still wins, so the
   three CI workflows that set `"false"` are unaffected. `R --vanilla` ignores
   `.Renviron`, and an explicit `"false"` still prunes.

2. **`.githooks/pre-commit` blocks the commit.** A pruned working tree is
   recoverable; a committed prune is the real loss, so the hook guards the
   commit. **It needs one command per clone:**

   ```bash
   git config core.hooksPath .githooks
   ```

   Without that, the hook does nothing. To retire a baseline deliberately, use
   `ALLOW_SNAPSHOT_DELETION=1 git commit ...`.

Note that the guard style in the test file is *not* a lever. `test_snapshots.R`
wraps most tests in a file-level `if (Sys.getenv(...) == "true")` and the rest
in `skip()` inside `test_that()`; on testthat 3.3.2 a prune takes the baselines
of both. Rewriting the guards would not have prevented any of this.

If a measurement genuinely needs the guard off (tracing under CRAN skip
semantics, say), run it against a throwaway export rather than this checkout —
`git archive HEAD | tar -x -C "$TMPDIR/tree"` — so pruning has nothing of
yours to delete.

Only the three CI workflows set `VDIFFR_RUN_TESTS: "false"` deliberately
(`R-CMD-check`, `test-coverage`, `check-manual`), where snapshots are not
regenerated and pruning has nothing to prune. That CI setting is why the hazard
is invisible until someone runs the suite on a laptop.

## Voice
Prose in vignettes, README, roxygen `@description`/`@details`, and release/post copy follows
the `ehrlinger-writing` harness — my voice, reader persona, project context. Apply it for any
documentation text in this repo.
