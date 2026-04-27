# Release Checklist: ggRandomForests v2.7.1
**Date:** 2026-04-27 | **Maintainer:** John Ehrlinger <john.ehrlinger@gmail.com>

---

## What Changed Since 2.7.0

Bug-fix release. See `NEWS.md` for the user-facing change log.

| Area | Fix |
|------|-----|
| `gg_partial_rfsrc.R` | Pass `partial.type = "surv"` (default) to `partial.rfsrc()` for survival forests; without it the C-level prediction routine aborted on a zero-length comparison and the survival vignette's partial-dependence chunks rendered empty. New `partial.type` argument accepts `"surv"` / `"chf"` / `"mort"`. |
| `gg_partial_rfsrc.R` | Reshape multi-`partial.time` results to long form so each `(x, time)` pair is one row (was crashing on the `time` column assignment when yhat came back as a matrix). |
| `plot.gg_partial.R` | Survival layout: predictor on x-axis, one curve per time point coloured by `Time`. Group/colour by full-precision time so distinct horizons that round to the same display value never collapse. Y-axis label adapts to `partial.type`. |
| `plot.gg_vimp.R` | Merge the duplicate "VIMP > 0" / "positive" legends by sharing a title across both `fill` and `color` aesthetics. |
| `R/ggrandomforests.news.R` | Read `NEWS.md` (the canonical change log) instead of the legacy hand-maintained `inst/NEWS`, which had silently drifted to v2.4.0 across three releases. `inst/NEWS` removed. |
| `R/calc_roc.R`, `R/plot.gg_roc.R` | Replace U+2212 Unicode minus with ASCII `-` so the PDF manual builds under standard LaTeX (CRAN's incoming pipeline). |
| `tests/testthat/test_plot_layer_data.R` | New regression suite using `ggplot2::layer_data()` to verify each `plot.gg_*()` method renders non-empty layers across all forest families; locks in the `partial.type` y-label dispatch and the merged-legend behaviour. |

---

## Pre-Release Checklist

### 1. Version & Metadata
- [x] Bump `Version:` in `DESCRIPTION` from `2.7.0.9001` → `2.7.1`
- [x] Update `Date:` in `DESCRIPTION` to `2026-04-27`
- [x] Confirm `RoxygenNote:` matches the installed roxygen2 version (`roxygen2::roxygenise()` ran clean)

### 2. NEWS.md
- [x] Add `ggRandomForests v2.7.1` section at the top of `NEWS.md`
- [x] Keep entries user-facing (each bullet says **what was wrong** and **what changed**)

### 3. Documentation
- [x] `roxygen2::roxygenise(".")` ran with zero warnings; all Rd files regenerate cleanly
- [x] Spot-checked the new `plot.gg_partial_rfsrc` and `gg_partial_rfsrc` help pages

### 4. Test Suite
- [x] Full local testthat run: 0 failures / 0 errors / 0 warnings
- [x] New `test_plot_layer_data.R` covers every `plot.gg_*()` method
- [x] New `plot.gg_vimp` legend test uses synthetic data (deterministic across CI platforms)

### 5. Validation
- [x] `R CMD check --as-cran` on macOS R 4.5.3: **0 errors | 0 warnings | 2 NOTEs** (incoming-feasibility + future-timestamps; both informational, neither actionable)
- [x] `urlchecker::url_check()`: 0 broken URLs
- [x] `spelling::spell_check_package()`: only domain-jargon items (rfsrc, VIMP, Ishwaran, etc.); no genuine typos
- [ ] `devtools::check_win_devel()` and `check_win_release()` — submit before tagging
- [ ] `revdepcheck::revdep_check()` — confirm 0 reverse deps still pass (ggRandomForests has none on CRAN, but run for completeness)
- [ ] All GitHub Actions matrix jobs green on the head commit (R-CMD-check standard / release / devel + windows + macos + lint)

### 6. CRAN comments
- [x] Update `cran-comments.md` with the actual environments tested and the disposition of NOTEs

### 7. Tag & Submit
- [ ] `git tag -a v2.7.1 -m "v2.7.1: bug-fix release"` and `git push origin v2.7.1`
- [ ] Upload tarball at https://cran.r-project.org/submit.html (or `devtools::submit_cran()`)
- [ ] Reply to CRAN's confirmation email within 24 h

### 8. After Acceptance
- [ ] Bump `DESCRIPTION` to `2.7.1.9000` and add a `v2.7.2 (in development)` placeholder at the top of `NEWS.md`
- [ ] `pkgdown` site rebuild fires automatically on tag push (existing GH Action) — verify https://ehrlinger.github.io/ggRandomForests/ shows v2.7.1 in the navbar
