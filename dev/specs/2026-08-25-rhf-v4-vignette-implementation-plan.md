# RHF v4 Vignette Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Publish a CRAN-safe longitudinal RHF vignette that follows one `randomForestRHF::hazard.simulation(1)` model through all four ggRandomForests RHF families and closes only the implementation-side RHF-vignette release gate.

**Architecture:** A reproducible preparation script fits the upstream model and calculates the upstream AUC, importance, and tuning objects once, then saves a versioned compressed bundle used during routine vignette builds. The vignette loads that bundle but runs every ggRandomForests extractor and plot live. Static and artifact-contract tests pin the single-data-source narrative, the time-dependent covariate identity, supplied-object workflow, citations, package integration, and size budget.

**Tech Stack:** R 4.4+, Quarto, knitr, ggplot2, testthat edition 2, roxygen2, pkgdown, randomForestRHF 1.0.1, devtools, lintr.

**Spec:** `dev/plans/2026-08-25-rhf-v4-consistency-tuning-vignette-design.md`

## Global Constraints

- Work on `codex/rhf-v4-vignette` and target the pull request to `dev_rhf`; do not merge, tag, submit to CRAN, merge to `main`, change the version, or release.
- The worked data source is only `randomForestRHF::hazard.simulation(1)`. Keep PBC in the existing randomForestSRC material and do not add a second RHF workflow.
- Use the simulation's actual column identity, `xtd = (x.4 + x.5) * stop`; do not reproduce the design document's schematic `x4`, `x5`, and `t` names as executable claims.
- Explain counting-process intervals, predictable covariate paths, and no-lookahead routing before interpreting model output.
- Distinguish cumulative/dynamic AUC from incident/dynamic AUC. Describe variable priority as a time-local rule-release contrast, never as a z-score, p-value, or thresholded selection statistic.
- Documentation is for a general CRAN R user. Follow `.claude/house-style.md` persona (d), assume R, ggplot2, and general random-forest familiarity, define RHF-specific ideas inline, and assume no institutional or biostatistics-group context.
- Lead the importance and tuning sections with retained, supplied upstream objects. The vignette may explain that importance can be calculated when omitted, but the documented default supplies the object.
- Routine rendering must not fit an RHF model or run upstream AUC, importance, or tuning calculations. Display preparation code in a non-evaluated chunk and stop clearly when the saved bundle is missing or invalid.
- All ggRandomForests extractors and plot methods run live against loaded upstream objects. Do not save ggRandomForests extractor results or rendered plots in the bundle.
- Keep one tracked `vignettes/rhf_precomputed.rds` bundle at or below 1.75 MiB. Do not create a general `tests/testthat/fixtures/` forest cache.
- Record the random seed, exact preparation settings, R version, ggRandomForests version, randomForestRHF version, and ggplot2 version in the bundle.
- Cite both `Ishwaran:RHF:2026` and `Ishwaran:RHF:software:2026` from the shared bibliography where their claims are made.
- Add no dependency. Keep `randomForestRHF (>= 1.0.1)` in `Suggests`, use package-qualified upstream calls, and never attach it from `R/`.
- Never hand-edit `NAMESPACE` or `man/`; regenerate them with `devtools::document()`.
- Every RNG-using `test_that()` block calls `set.seed()` inside that block. Slow work calls `skip_on_cran()`.
- Every full suite run is exactly `NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::test()'`, with scoped snapshot status and diff checked immediately before and after.
- Mark only the RHF-vignette implementation gate verified in `release-checklist-v4.0.0.md`. Keep full release verification, explicit authorization, submission, and CRAN acceptance pending.

---

### Task 1: Define and generate the precomputed RHF bundle

**Files:**
- Create: `vignettes/precompute_rhf.R`
- Create: `tests/testthat/test_rhf_vignette_assets.R`
- Generate: `vignettes/rhf_precomputed.rds`
- Reference: `vignettes/precompute_varpro.R`
- Reference: `tests/testthat/helper-rhf-fixtures.R`

**Bundle contract:**
- Contains: `data`, `fit`, `auct_cumulative`, `auct_incident`, `importance`, `tune_risk`, `tune_iauc`, `seed`, `settings`, and `versions`.
- Every derived object comes from the stored `fit`, whose model data are the stored `data` from one `hazard.simulation(1)` call.
- Tuning objects are saved with `forest = FALSE`; the bundle is compressed with `xz`.

- [ ] **Step 1: Write a failing artifact-contract test**

Create `tests/testthat/test_rhf_vignette_assets.R`. Load the bundle from `vignettes/rhf_precomputed.rds`, require the exact top-level names above, and check:

```r
expect_lte(file.info(path)$size, 1.75 * 1024^2)
expect_s3_class(bundle$fit, "rhf")
expect_s3_class(bundle$tune_risk, "tune.treesize.rhf")
expect_s3_class(bundle$tune_iauc, "tune.treesize.rhf")
expect_false("forest" %in% names(bundle$tune_risk))
expect_false("forest" %in% names(bundle$tune_iauc))
expect_equal(
  bundle$data$xtd,
  (bundle$data$x.4 + bundle$data$x.5) * bundle$data$stop
)
expect_gt(length(unique(bundle$data$id)), 1L)
expect_true(any(duplicated(bundle$data$id)))
expect_true(all(bundle$data$start < bundle$data$stop))
```

Also verify `auct_cumulative$method == "cumulative"`,
`auct_cumulative$marker == "cumhaz"`, `auct_incident$method == "incident"`,
`auct_incident$marker == "hazard"`, five valid importance time indices,
`tune_risk$perf == "risk"`, `tune_iauc$perf == "iAUC"`, scalar seed, and
nonempty named version strings. Require `settings` to contain exactly
`formula`, `fit`, `auct_cumulative`, `auct_incident`, `importance_cache`,
`importance_time_index`, `tune_risk`, and `tune_iauc`. Use structural or
semantic checks instead of serialized-object byte comparisons.

- [ ] **Step 2: Run the focused test and confirm the RED state**

```bash
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test_rhf_vignette_assets.R")'
```

Expected: failure reports the missing bundle; no snapshot file changes.

- [ ] **Step 3: Write the reproducible preparation script**

Create `vignettes/precompute_rhf.R` with these fixed inputs:

```r
seed <- 20260825L
set.seed(seed)
sim <- randomForestRHF::hazard.simulation(1)
formula <- stats::as.formula("Surv(id, start, stop, event) ~ .")
fit <- randomForestRHF::rhf(
  formula, sim$dta, ntree = 50L, seed = -1L
)
auct_cumulative <- randomForestRHF::auct.rhf(
  fit, marker = "cumhaz", method = "cumulative", verbose = FALSE
)
auct_incident <- randomForestRHF::auct.rhf(
  fit, marker = "hazard", method = "incident",
  riskset = "subject", verbose = FALSE
)
cache <- randomForestRHF::varpro.cache.rhf(
  fit, max.rules.tree = 30L, max.tree = 20L, verbose = FALSE
)
time_index <- unique(as.integer(round(seq.int(1L, cache$K, length.out = 5L))))
importance <- randomForestRHF::importance.rhf(
  fit, cache = cache, time.index = time_index, verbose = FALSE
)
tune_risk <- randomForestRHF::tune.treesize.rhf(
  formula, sim$dta, ntree = 20L, perf = "risk", lower = 2L,
  upper = 6L, max.evals = 5L, seed = seed, verbose = FALSE,
  forest = FALSE
)
tune_iauc <- randomForestRHF::tune.iAUC.rhf(
  formula, sim$dta, ntree = 20L, lower = 2L, upper = 6L,
  max.evals = 5L, seed = seed, verbose = FALSE, forest = FALSE
)
```

Build the named `settings` entries from the literal arguments above and record
named `versions` entries for `R`, `ggRandomForests`, `randomForestRHF`, and
`ggplot2`. Before saving, assert the `xtd` identity, class contracts, both
tuning objects' absence of a `forest` component, and the size budget. Save
only the final bundle with `saveRDS(..., compress = "xz")`; do not save
`cache`.

- [ ] **Step 4: Generate the artifact and make the focused test GREEN**

```bash
Rscript vignettes/precompute_rhf.R
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test_rhf_vignette_assets.R")'
```

Expected: the artifact is no larger than 1.75 MiB and every bundle-contract expectation passes.

- [ ] **Step 5: Review the artifact without regenerating it**

Run a read-only summary that reports top-level names, classes, row/subject counts, event subjects, interval range, AUC methods, importance time indices, tuning path sizes, versions, and compressed bytes. Confirm that the bundle contains no ggRandomForests output objects and no tuning forest.

- [ ] **Step 6: Commit the bundle task**

```bash
git diff --check
git add vignettes/precompute_rhf.R vignettes/rhf_precomputed.rds tests/testthat/test_rhf_vignette_assets.R
git commit -m "docs: prepare longitudinal RHF vignette data"
```

---

### Task 2: Build the vignette foundation and hazard story

**Files:**
- Create: `vignettes/rhf.qmd`
- Create: `tests/testthat/test_rhf_vignette_source.R`
- Reference: `vignettes/ggRandomForests.qmd`
- Reference: `vignettes/varpro.qmd`
- Reference: `vignettes/_fig_optim.R`

- [ ] **Step 1: Write failing source-contract tests**

Read `vignettes/rhf.qmd` as text and require:

- title `Random Hazard Forests with ggRandomForests` and standard CRAN vignette metadata;
- the shared bibliography and both RHF citation keys;
- a loader for `rhf_precomputed.rds` that stops if the artifact cannot be read;
- exactly one displayed `randomForestRHF::hazard.simulation(1)` data source;
- the literal executable identity `(x.4 + x.5) * stop`;
- the terms `counting-process`, `predictable`, and `no-lookahead` in reader-facing prose;
- no `pbc` or `PBC` occurrence;
- no evaluated call to `rhf()`, `auct.rhf()`, `importance.rhf()`, `tune.treesize.rhf()`, or `tune.iAUC.rhf()`.

Parse fenced chunk options sufficiently to distinguish the displayed `eval: false` preparation chunk from evaluated chunks; do not merely ban the upstream function names globally.

- [ ] **Step 2: Run the focused source test and confirm RED**

Use the focused command from Task 1 with `test_rhf_vignette_source.R`.

Expected: failure reports the missing `vignettes/rhf.qmd`.

- [ ] **Step 3: Create YAML, setup, loader, and reproducibility boundary**

Follow current QMD conventions: source the figure optimizer, set single-core options, load only needed namespaces, and define two candidate paths (`rhf_precomputed.rds` and `vignettes/rhf_precomputed.rds`). Stop with a plain message that names `vignettes/precompute_rhf.R` if neither path exists or the contract is invalid. Do not fall back to live fitting.

Add an `eval: false` preparation chunk showing the fixed calls from Task 1. Tell the reader that the saved objects keep CRAN builds predictable and that they may rerun the script to reproduce them.

- [ ] **Step 4: Write the data and model sections in CRAN-user voice**

Open from the familiar idea that a baseline row cannot show a covariate changing over follow-up. Explain one subject's repeated `(start, stop]` intervals, why `xtd` uses the value available at `stop`, and how predictable/no-lookahead routing prevents future measurements from selecting an earlier branch. State inherited upstream behavior as randomForestRHF behavior.

Show compact summaries of subjects, records per subject, events, and `xtd`; avoid dumping the full data. Display the package-qualified RHF fit code in the non-evaluated preparation block and inspect the loaded `fit` in evaluated code.

- [ ] **Step 5: Add live hazard and cumulative-hazard extraction**

Run `gg_rhf(bundle$fit)` live and show the established hazard and CHF plot methods using their actual API. Explain hazard as local event rate and cumulative hazard as accumulated event pressure without treating either as a probability.

- [ ] **Step 6: Render, inspect, and make source tests GREEN**

```bash
Rscript -e 'quarto::quarto_render("vignettes/rhf.qmd")'
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test_rhf_vignette_source.R")'
```

Inspect the rendered article for clipped figures, raw warnings, unreadable tables, duplicated setup output, and unexplained upstream terms.

- [ ] **Step 7: Commit the foundation**

```bash
git diff --check
git add vignettes/rhf.qmd tests/testthat/test_rhf_vignette_source.R
git commit -m "docs: start longitudinal RHF vignette"
```

---

### Task 3: Complete all four RHF workflows and interpretation

**Files:**
- Modify: `vignettes/rhf.qmd`
- Modify: `tests/testthat/test_rhf_vignette_source.R`
- Reference: `R/gg_auct.R`
- Reference: `R/gg_rhf_importance.R`
- Reference: `R/gg_tune_rhf.R`

- [ ] **Step 1: Extend source tests for the complete narrative**

Require live calls that consume the saved upstream objects:

```r
gg_auct(bundle$fit, marker = "chf", auct_fit = bundle$auct_cumulative)
gg_auct(bundle$fit, marker = "haz", auct_fit = bundle$auct_incident)
gg_rhf_importance(bundle$fit, importance_fit = bundle$importance)
gg_tune_rhf(bundle$tune_risk)
gg_tune_rhf(bundle$tune_iauc)
```

Require reader-facing text that distinguishes cumulative/dynamic from
incident/dynamic AUC, states that priority is a time-local rule-release
contrast, recommends retaining and supplying the upstream importance/tuning
objects, and says that the scores are not z-scores, p-values, or automatic
selection thresholds.

Require a support table naming all four extractor families, their upstream input classes, returned ggRandomForests classes, and the scale on which each output is read. Require a further-reading section with both RHF citations.

- [ ] **Step 2: Run focused tests and confirm RED**

Expected: the foundation remains valid while missing workflow and interpretation expectations fail.

- [ ] **Step 3: Add cumulative and incident AUC sections**

Use the stored `auct_cumulative` and `auct_incident` objects. Explain that cumulative/dynamic AUC asks whether cases observed by a horizon rank above those still at risk, while incident/dynamic AUC asks about events occurring near that time among the relevant risk set. Name marker differences and do not imply the curves estimate the same target.

- [ ] **Step 4: Add the supplied variable-priority workflow**

Lead with the precomputed `importance` object and supply it to `gg_rhf_importance()` with the loaded fit. Explain the cache/calculation step as upstream work performed once. Interpret signs and magnitude only according to the verified RHF API; do not invent significance thresholds.

- [ ] **Step 5: Add risk and iAUC tuning workflows**

Supply `tune_risk` and `tune_iauc` to `gg_tune_rhf()`. Explain that the path preserves upstream evaluation order, the point marks the upstream selected tree size, risk has no fabricated uncertainty, and an iAUC ribbon appears only when finite upstream standard errors exist.

- [ ] **Step 6: Add the support table, reproducibility, and citations**

Summarize `gg_rhf`, `gg_auct`, `gg_rhf_importance`, and `gg_tune_rhf` without marketing language. Close with how to regenerate the bundle, why supplied objects are the default, relevant package versions from `bundle$versions`, and both RHF method/software citations.

- [ ] **Step 7: Render, inspect, and make focused tests GREEN**

Run the render and focused tests from Task 2. Read the article in order as a CRAN user, checking that each figure answers the question immediately preceding it and that the prose does not assume the reader belongs to a biostatistics group.

- [ ] **Step 8: Commit the complete workflow**

```bash
git diff --check
git add vignettes/rhf.qmd tests/testthat/test_rhf_vignette_source.R
git commit -m "docs: complete RHF vignette workflow"
```

---

### Task 4: Integrate the vignette across public documentation

**Files:**
- Modify: `_pkgdown.yml`
- Modify: `README.md`
- Modify: `R/help.R`
- Modify: `NEWS.md`
- Modify: `release-checklist-v4.0.0.md`
- Generated: `man/ggRandomForests-package.Rd`
- Audit: `vignettes/ggRandomForests.bib`

- [ ] **Step 1: Add failing integration assertions**

Extend `test_rhf_vignette_source.R` or add a focused documentation-inventory block requiring:

- `rhf` in the pkgdown articles list and tutorials navbar;
- a README link using `vignette("rhf", package = "ggRandomForests")`;
- a package-help pointer to the RHF vignette;
- a v4 NEWS bullet for the longitudinal four-family workflow;
- both existing bibliography keys with the method DOI `10.48550/arXiv.2608.21597` and software version `1.0.1`;
- one release-checklist RHF-vignette row/gate that can be marked only after rendering and review.

- [ ] **Step 2: Run the focused test and confirm RED**

Expected: vignette content tests pass; public-integration expectations fail.

- [ ] **Step 3: Add public links and concise release prose**

Add the article to `_pkgdown.yml`, README, and package overview. Update NEWS in the terse register. Audit the shared bibliography; retain it unchanged if both entries are already exact. Add no duplicate citations.

- [ ] **Step 4: Update the checklist without over-closing gates**

Change the documentation-audit RHF-vignette row from deferred to corrected only after the article renders and has been reviewed. Mark the `RHF vignette` release gate verified with concrete files and command evidence. Keep `Full release verification`, `Explicit maintainer authorization`, `Submission`, and `CRAN acceptance` unchecked and pending.

- [ ] **Step 5: Regenerate documentation and make integration tests GREEN**

```bash
Rscript -e 'devtools::document()'
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test_rhf_vignette_source.R")'
```

- [ ] **Step 6: Run the cross-family consistency inventory**

Search README, package help source, active v4 NEWS, every QMD, and `_pkgdown.yml` for `rfsrc`, `rhf`, `varpro`, package names, object classes, version numbers, and citation keys. Verify the canonical mappings remain:

| Package | Fit call | Object class |
|---|---|---|
| `randomForestSRC` | `randomForestSRC::rfsrc()` | `rfsrc` |
| `randomForestRHF` | `randomForestRHF::rhf()` | `rhf` |
| `varPro` | `varPro::varpro()` | `varpro` |

Correct only inconsistencies introduced by or directly exposed through the new vignette integration; record unrelated findings instead of widening PR 3.

- [ ] **Step 7: Commit public integration**

```bash
git diff --check
git add _pkgdown.yml README.md R/help.R NEWS.md release-checklist-v4.0.0.md man/ggRandomForests-package.Rd
git commit -m "docs: publish RHF vignette"
```

---

### Task 5: Verify rendering, package behavior, and site integration

**Files:**
- Modify only if verification finds an in-scope defect: files from Tasks 1–4
- Do not commit: rendered site output, temporary caches, check directories, or tarballs

- [ ] **Step 1: Start from a clean snapshot baseline**

```bash
git status --short
git status --short tests/testthat/_snaps
git diff --name-status -- tests/testthat/_snaps
```

Expected: no unintended working-tree changes and no snapshot delta.

- [ ] **Step 2: Run definition-of-done commands in required order**

```bash
Rscript -e 'devtools::document()'
Rscript -e 'lintr::lint_package()'
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::test()'
```

Record exact exit status, passes, failures, errors, warnings, skips, and duration. Immediately rerun the two scoped snapshot commands and require no changed or deleted baseline.

- [ ] **Step 3: Run spelling and all seven vignette renders**

```bash
Rscript -e 'spelling::spell_check_package(use_wordlist = TRUE)'
Rscript -e 'for (x in list.files("vignettes", pattern = "[.]qmd$", full.names = TRUE)) quarto::quarto_render(x)'
```

Use an isolated temporary home if the known Quarto cache requires it. Review reported words rather than automatically adding technical terms. Inspect the RHF article and at least the changed navigation context visually.

- [ ] **Step 4: Build pkgdown**

```bash
Rscript -e 'pkgdown::build_site()'
```

Confirm the tutorial navbar and articles index reach the RHF article, its citations resolve, figures render, and no generated site files are staged. If restricted DNS blocks an essential remote asset, rerun only the failed command with the required approval.

- [ ] **Step 5: Confirm runtime and size budgets**

Report `vignettes/rhf_precomputed.rds` bytes, time the RHF vignette render, and compare the final source tarball size with the repository's CRAN budget. Stop if the artifact exceeds 1.75 MiB or the source tarball is 5 MiB or larger.

- [ ] **Step 6: Fix only verified in-scope failures and repeat affected gates**

Use systematic debugging for any unexpected failure. After a source change, restart the definition-of-done sequence at `devtools::document()` and repeat any downstream render/site check affected by the edit.

---

### Task 6: Run the PR-level archive check and prepare the pull request

**Files:**
- Modify: `release-checklist-v4.0.0.md`
- Do not commit: archive export, tarball, check directory, or logs

- [ ] **Step 1: Build from a clean `git archive` export**

Create a temporary directory with `mktemp -d`, export `HEAD`, and run `R CMD build` from that export with the manual and the established isolated-home workaround. Do not build the PR-level artifact from the working tree.

- [ ] **Step 2: Inspect the tarball before checking it**

```bash
tar tzf ggRandomForests_4.0.0.tar.gz | grep -E '/\.[^/]+'
tar xzf ggRandomForests_4.0.0.tar.gz -O ggRandomForests/DESCRIPTION | sed -n '4,5p'
tar tzf ggRandomForests_4.0.0.tar.gz | grep -c cran-comments
```

Expected: only `ggRandomForests/.Rinstignore`; Version `4.0.0`; Date `2026-08-05`; zero `cran-comments`; source tarball remains below 5 MiB.

- [ ] **Step 3: Run the manual-inclusive CRAN check**

```bash
R CMD check --as-cran ggRandomForests_4.0.0.tar.gz
```

Do not pass `--no-manual`. Record exact errors, warnings, and notes. The known incoming-feasibility timing/update NOTE may be documented but must not be silently described as 0 notes.

- [ ] **Step 4: Record PR 3 evidence and keep release holds visible**

Add a chronological PR 3 verification section containing artifact size, seven-vignette render result, pkgdown result, suite totals, snapshot integrity, tarball inspection, and check result. Confirm again that only the RHF-vignette implementation gate advances.

- [ ] **Step 5: Repeat documentation, lint, and guarded tests after the checklist edit**

Follow the required order. If only Markdown evidence changed, a fresh archive check is not required unless build inputs or generated documentation changed; state which earlier archive evidence remains applicable.

- [ ] **Step 6: Perform a final self-review**

Check the implementation against every design requirement and every Global Constraint. Search for `TBD`, `TODO`, placeholder citations, PBC, institutional language, evaluated expensive calls, stale `xtd` notation, unsupported statistical claims, duplicated bibliography entries, and accidentally checked release gates. Confirm all documented functions, classes, columns, bundle keys, version strings, and file paths match their implementation.

- [ ] **Step 7: Commit verification evidence**

```bash
git diff --check
git add release-checklist-v4.0.0.md
git commit -m "docs: record RHF vignette verification"
```

- [ ] **Step 8: Push and open the PR to `dev_rhf`**

```bash
git status --short --branch
git log --oneline origin/dev_rhf..HEAD
git push -u origin codex/rhf-v4-vignette
gh pr create --base dev_rhf --head codex/rhf-v4-vignette
```

The PR summary must state that it closes the implementation-side RHF-vignette gate but does not authorize a release, CRAN submission, tag, version change, merge to `main`, or closure of CRAN acceptance. Stop after opening the PR for maintainer review.

## Plan completion criteria

- The tracked, reproducible bundle is at most 1.75 MiB and satisfies its artifact contract.
- `vignettes/rhf.qmd` renders without live upstream fitting and uses one longitudinal simulation through all four RHF families.
- The narrative accurately teaches counting-process input, predictable/no-lookahead behavior, AUC target differences, and time-local RHF priority to a general CRAN user.
- Importance and tuning lead with supplied upstream objects; ggRandomForests extraction and plotting remain live.
- README, package help, NEWS, pkgdown, bibliography, and release checklist are consistent and linked.
- Documentation, lint, guarded tests, snapshot integrity, spelling, seven vignette renders, pkgdown, archive inspection, and manual-inclusive `R CMD check --as-cran` have fresh recorded evidence.
- Only the RHF-vignette implementation gate is newly verified; all release, authorization, submission, and CRAN-acceptance holds remain pending.
