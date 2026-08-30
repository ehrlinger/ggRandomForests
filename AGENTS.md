# ggRandomForests

CRAN R package. A visualisation and exploration layer over `randomForestSRC`, `randomForest`
and `varPro` objects, built on ggplot2. The public API is the `gg_*` extractor family plus
their `plot()` and `autoplot()` methods (19 of each, as declared in `NAMESPACE`).

This file is the operational contract for any agent working in this repo. It is deliberately
short. Structure, the `gg_*` design pattern, roxygen standards and code style are documented
once in `CONTRIBUTING.md`; read that rather than expecting them restated here.

## Definition of done

A change is not done until these pass, in this order:

```bash
Rscript -e 'devtools::document()'
Rscript -e 'lintr::lint_package()'                                  # must be 0 lints
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::test()'    # 0 failures, 0 errors
```

Then, once per PR rather than once per edit:

```bash
R CMD check --as-cran   # with the manual; do not pass --no-manual
```

Run the commands. Reading the code is not evidence, and neither is a subagent's report.

Three details in that list are load bearing:

- **`document()` runs first, every time.** `man/` and `NAMESPACE` are generated. A stale
  `NAMESPACE` makes the test run answer a question about the previous commit.
- **Lint runs before tests** because it costs about 17 seconds against about 110 for the
  suite. Cheap failures first.
- **The test command needs both environment variables.** See "The one thing that destroys
  work" below. `devtools::test()` sets `NOT_CRAN=true` itself, but a bare
  `testthat::test_file()` under `load_all()` does not, so every `skip_on_cran()` test
  silently skips and the run reports `SS` instead of a result. Reading `SS` as "fine" is how
  you conclude a test passed when it never ran. There are 37 `skip_on_cran()` calls in this
  suite, so this is not a hypothetical.

`R CMD check` runs with `NOT_CRAN` false, so it does **not** exercise those 37 tests. A green
check is not evidence that they pass; only the `devtools::test()` line above is.

## The one thing that destroys work

**A suite run with `VDIFFR_RUN_TESTS` unset deletes every vdiffr baseline as "unused."**
There are 58 of them under `tests/testthat/_snaps/snapshots/`, and they are the package's
only visual regression coverage.

Always run the suite as:

```bash
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::test()'
```

Check `git status` before and after any local suite run. A wave of staged snapshot deletions
is a signal that the variable was missing, not a change to commit.

This has happened repeatedly, so two protections now sit under that advice:

1. **`.Renviron` inverts the default.** It sets `VDIFFR_RUN_TESTS=${VDIFFR_RUN_TESTS-true}`,
   so an unset variable (the natural state of every fresh shell, script and agent session,
   and the cause of every prune so far) now reads as `true`. An explicit value still wins, so
   the three CI workflows that set `"false"` are unaffected. Note that `R --vanilla` ignores
   `.Renviron`, and an explicit `"false"` still prunes.

2. **`.githooks/pre-commit` blocks the commit.** A pruned working tree is recoverable; a
   committed prune is the real loss. **This needs one command per clone:**

   ```bash
   git config core.hooksPath .githooks
   ```

   Without it the hook does nothing. To retire a baseline deliberately, use
   `ALLOW_SNAPSHOT_DELETION=1 git commit ...`.

Further notes, so nobody re-derives them:

- If you regenerate a baseline, do it **last**. A later full-suite run deletes it, and a
  blanket `git checkout -- tests/testthat/_snaps/` to undo that silently reverts your
  regeneration along with the pruning.
- **That rule is branch-local, and the merge case is the one it misses.** A branch that
  adds baselines and a branch that changes what those baselines render are both green on
  their own, and the files never textually conflict, so nothing fails until the second
  one merges. It happened on 2026-08-29: #252 added four `plot.gg_variable()` survival
  baselines while #250, which removed the hard-coded `"year"` from that same axis title,
  sat in review. Two of the four baked in `Survival at 1 year`, and #250 would have
  merged clean and left `main` quietly wrong: green in CI, which never compares an SVG,
  and failing only for whoever next ran the suite locally with the guard on. **Before
  merging a PR that changes rendered output, re-check `main` for baselines added since
  you branched**, then merge `main` in, regenerate, and push.
  `git diff --stat <merge-commit>..HEAD` afterwards should name only the baselines you
  meant to touch.
- All 58 baselines are tracked today. That was not true on 2026-08-06, when one unguarded run
  pruned 49 files and the 9 untracked ones survived only because a stale copy happened to
  remain in `ggRandomForests.Rcheck/00_pkg_src/`. That is not a backup and will not reliably
  be there.
- The guard style in the test file is **not** a lever. `test_snapshots.R` wraps most tests in
  a file-level `if (Sys.getenv(...) == "true")` and the rest in `skip()` inside `test_that()`;
  on testthat 3.3.2 a prune takes the baselines of both. Rewriting the guards would not have
  prevented any of this.
- If a measurement genuinely needs the guard off (tracing under CRAN skip semantics, say), run
  it against a throwaway export rather than this checkout:
  `git archive HEAD | tar -x -C "$TMPDIR/tree"`, so pruning has nothing of yours to delete.
- Only `R-CMD-check`, `test-coverage` and `check-manual` set `VDIFFR_RUN_TESTS: "false"`
  deliberately, where snapshots are not regenerated and pruning has nothing to prune. That CI
  setting is why the hazard stays invisible until someone runs the suite on a laptop.

## The automated gates

Three layers, at three different moments. They are complementary, not redundant.

| Gate | When | What it runs |
|---|---|---|
| `.claude/hooks/verify.sh` (Claude Code `Stop` hook) | session end | lint, plus the test files matching what changed, plus the six cross-cutting files. About 25 seconds, or about 130 when a changed file maps to no test and it falls back to the whole suite |
| `.githooks/pre-commit` | `git commit` | blocks a commit that deletes vdiffr baselines |
| CI, and `R CMD check --as-cran` | PR | everything |

The `Stop` hook deliberately does **not** run the full suite. At about 109
seconds the suite's cost is the code under test, not setup, so a full-suite gate
would add roughly 126 seconds to every session end, and a gate that expensive
gets switched off. It is allowed to miss a cross-file breakage that CI catches.

There is **no auto-formatting hook.** `styler` was measured against this repo
and would rewrite 48 of 51 files in `R/`, 2917 diff lines, so a one-line edit
would come back with hundreds of lines of unrelated reformatting attached. Lint
is the style gate here, and it is already at zero.

Neither hook is a substitute for running the definition of done yourself.

## Before you touch code

Orient on the public API surface and where things live **before** editing. Do not infer the
structure of this package from a partial file read: `NAMESPACE` carries 29 `export()`
directives and 115 `S3method()` registrations across 19 extractor families, and the S3
dispatch layer means the function you found is often not the one that runs.

## Generated files: never hand-edit

| Path | Generated by |
|---|---|
| `man/`, `NAMESPACE` | roxygen2, via `devtools::document()` |
| `.claude/house-style.md` | the `ehrlinger/house-style` composer |

`.claude/house-style.md` carries a `DO NOT EDIT` banner and the `house-style` CI job **fails
the build** when it drifts from its vault sources. Editing it reddens CI and the next
recompose reverts you.

## Rules for this repo

- `gg_*` functions return an object. `plot()` and `autoplot()` methods **return** a ggplot
  object; they never `print()` it.
- **Changing the class, element names or column names of a returned object is a breaking
  change.** This package is on CRAN. Check reverse dependencies before proposing one.
- **Importance plots put the most-important variable at the top.** After `coord_flip()` that
  means it is the *last* factor level of the variable axis. `test_plot_conventions.R` pins
  this across `gg_vimp`, `gg_varpro`, `gg_beta_varpro` and `gg_ivarpro`, because a
  bottom-heavy ordering was a real bug once.
- **`Depends` carries only the R version constraint.** `randomForestSRC`, `randomForest` and
  `varPro` are `Imports` and are never attached from `R/`. `test_namespace_hygiene.R` pins
  this. (`tests/testthat/setup.R` attaches them for the tests only; that is not licence to do
  it in `R/`.)
- Every `plot()` / `autoplot()` method should have a `vdiffr::expect_doppelganger()` test in
  `test_snapshots.R`. There are 58 today against 38 methods; coverage is broad but has not
  been audited per method.
- **Tests are deterministic, and every `test_that()` block that touches the RNG calls
  `set.seed()` inside that block.** A file-level seed does not count: testthat promises no
  execution order, and every earlier block advances the stream. `test_determinism.R` pins
  this by parsing the suite, so a new unseeded block fails a test rather than passing review.
- Anything slow gets `skip_on_cran()`.
- No `browser()`, no bare `print()`, no `library()` inside `R/`.

### Where the suite's time actually goes

Roughly 110 seconds, and it is **not** the forest fits. Measured 2026-08-17:

| Component | Time | Note |
|---|---|---|
| Two `gg_partial_varpro` tests (surv, rmst) | 47 s | `partialpro()` is the function under test |
| `test_snapshots.R` | 17 s | vdiffr SVG rendering |
| `test_gg_udependent.R` | 12 s | `get.beta.entropy()`, already memoised per signature |
| Everything else, including about 200 forest fits | about 34 s | spread thin |

Fits are cheap: `test_gg_rfsrc.R` has 61 of them and runs in 1.9 seconds, while
`test_gg_partial_varpro.R` has 3 and runs in 59. Inside the worst test the fit is 0.2 s
against 22 s of `partialpro()`, so **the setup is 1 percent of the cost.**

Two consequences worth not re-deriving:

- **Do not build a `tests/testthat/fixtures/` directory of saved forests.** It cannot reach
  the real cost, and it is expensive: one survival forest is 1.2 MB with gzip and 314 KB with
  xz, against a 2.4 MB tarball and CRAN's 5 MB limit. It would also pin serialised
  `randomForestSRC` internals to one version. Use session-memoised helpers instead, as
  `helper-varpro-fixtures.R` and `test_gg_udependent.R` already do.
- **The suite is near its floor.** Anything that needs a fast local gate should run lint plus
  the tests for the changed files, not the whole suite.

Still, prefer reusing an existing fixture or memoised helper over adding another inline fit,
and keep any new fit small (few trees, few rows). It exists to exercise a code path, not to be
statistically realistic.

## Change discipline

1. **Think before coding.** Do not assume, ask. If the request is ambiguous or a name, path or
   signature is uncertain, surface the confusion instead of running with a guess. One good
   clarifying question beats a confident wrong edit.
2. **Simplicity first.** Write the minimum code that solves the stated problem. No speculative
   abstractions, no "while I'm here" generalising. For this scientific code, prefer the plain
   readable form a future reader can follow over the clever one.
3. **Surgical changes.** Touch only what the task requires. Do not refactor, reformat or
   re-style adjacent code, and do not reorganise imports or rename things that were not asked
   for. If you spot something worth changing nearby, note it separately rather than folding it
   in.
4. **Define "done" as a passing test.** State what done looks like before you start. If no
   test covers the change, add or propose one rather than declaring success from inspection.

A new dependency is a CRAN cost. Ask first.

## Git and versioning

- **Never push to `main`.** Branch, commit, push the branch, open a PR, then stop. The
  maintainer merges.
- **Never roll the MINOR or MAJOR digit.** That is the maintainer's call, made when a feature
  set is consolidated into a release. Patch bumps (`3.5.1` to `3.5.2`) are fine for
  incremental work; say so when you make one.
- **Always a plain three-digit version.** No `.9000` suffix, no fourth digit.
- Every version bump updates **both** `DESCRIPTION` and the `Version:` line in `NEWS.md`. A
  test greps `NEWS.md` for the exact `DESCRIPTION` version.

## Prose

Documentation prose (vignettes, README, roxygen `@description` and `@details`, release copy)
follows the house style in `.claude/house-style.md`: a specific voice, reader persona and
project context. Read it before writing user-facing text.

## Gotchas

- `object_usage_linter` is currently **disabled** in `.lintr`, so lint will not catch an
  undefined symbol or an unused local. Do not rely on a green lint for that class of error.
- `testthat` runs on **edition 2** here: `DESCRIPTION` has no `Config/testthat/edition` field.
  Do not assume 3rd-edition semantics.
- `randomForestSRC` output structure varies by version (3.6.2 is installed; `DESCRIPTION`
  requires `>= 3.4.0`). Never index its fields by position.
- CRAN rejects a package whose overall `R CMD check` exceeds about 10 minutes even at 0/0/0,
  and the rule bites at the **incoming pretest**, not per-flavor afterwards. The released
  3.5.0 sat at 673s on CRAN's own `r-devel-windows` marked OK, while 3.5.1 at 720s was
  declined, so a green CRAN check results page is not evidence of headroom. Measured on
  3.5.2 (2026-08-20): locally about 100s of timed steps, vignette rebuild 37s,
  `--run-donttest` examples 32s, tests 14s; on win-builder, 265s of timed steps on r-devel
  and 277s on r-oldrelease, against 608s for the 3.5.1 pretest.
  win-builder runs roughly 5x a local macOS box on tests and vignettes and about 3x on examples,
  so multiply per step rather than applying one factor. Watch that budget when adding to
  either.
- Build `R CMD check` from a clean `git archive` export, not the working tree. An empty
  `inst/doc` fabricates two vignette WARNINGs, and in a git worktree `.git` is a *file*, so
  the VCS exclusion misses it and it lands in the tarball as a spurious hidden-files NOTE.
  Both look like package defects and are not.
- **Verify the tarball before it leaves, rather than reasoning about `.Rbuildignore`.** The
  release-gate check is one line:

  ```bash
  tar tzf ggRandomForests_<version>.tar.gz | grep -E '/\.[^/]+'
  ```

  Anything other than `ggRandomForests/.Rinstignore` means stop and rebuild. This is the
  check that would have caught the `.remember` directory win-builder reported on
  2026-08-18. Also confirm `Version`/`Date` and that `cran-comments.md` is absent:

  ```bash
  tar xzf ggRandomForests_<version>.tar.gz -O ggRandomForests/DESCRIPTION | sed -n '4,5p'
  tar tzf ggRandomForests_<version>.tar.gz | grep -c cran-comments   # expect 0
  ```
- A working-tree `R CMD build .` is **not** a reason to rebuild on its own. Measured
  2026-08-20 at `26416c6c`: a build from the full working tree (71 MB of untracked
  `.Rcheck`, `docs/`, `.claude/`, `.remember/`, `.Rproj.user/`) and a build from a clean
  `git archive` export produced tarballs with an identical 247-entry file list, no
  difference in either direction. The `(^|/)\.remember$`, `(^|/)\.Rhistory$` and
  `(^|/)\.DS_Store$` guards at the foot of `.Rbuildignore` close the 2026-08-18 hole, and
  `R CMD build` prunes matched *directories* wholesale, so `.claude/settings.local.json` and
  the `.Rcheck` tree are never walked. Testing an ignore pattern against a full file path
  wrongly reports those two as leaks. Run the `tar tzf` check above instead of predicting.
