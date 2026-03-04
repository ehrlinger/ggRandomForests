# ggRandomForests Package Memory

## Project Overview
- R package: `ggRandomForests` — ggplot2-based visualizations for random forests
- Supports: classification, regression, and survival forests (randomForestSRC + randomForest)
- Working dir: `.claude/worktrees/lucid-herschel/`

## Key Architecture
- `R/gg_*.R` — data extraction functions (return `gg_*` classed data.frames)
- `R/plot.gg_*.R` — S3 plot methods for each `gg_*` class
- `R/gg_partial*.R` + `R/surv_partial.rfsrc.R` — partial dependence functions

## Test Coverage History
- Before: 62.36% overall, six files at 0%
- After (lucid-herschel branch): **83.33%** overall
- Files brought to 100%: `gg_partial.R`, `gg_partialpro.R`, `gg_survival.R`,
  `ggrandomforests.news.R`, `kaplan.R`, `surv_partial.rfsrc.R`, `varpro_feature_names.R`, `zzz.R`
- `gg_partial_rfsrc.R`: 0% → 96.61%

## Test Files Added/Modified
- **NEW**: `test_gg_partial.R` — unit tests for `gg_partial` + integration for `gg_partial_rfsrc`
- **NEW**: `test_gg_partialpro.R` — unit tests for `gg_partialpro`
- **NEW**: `test_varpro_feature_names.R` — unit tests for `varpro_feature_names`
- **NEW**: `test_surv_partial.R` — integration tests for `surv_partial.rfsrc`
- **NEW**: `test_ggrandomforests_news.R` — tests for `ggrandomforests.news`
- **EXPANDED**: `test_gg_rfsrc.R` — added survival forest tests (conf.int, by, surv_type)
- **EXPANDED**: `test_gg_variable.R` — added survival/regression/classification plot branches

## Critical Patterns
- Survival formula: MUST use `Surv <- survival::Surv` before `rfsrc()` call
  (NOT `survival::Surv()` inline in formula — rfsrc parser rejects it)
- `gg_partial_rfsrc` input validation returns a string (not throws error)
- `gg_partialpro` categorical rows = n_obs × n_categories (one row per obs per category)
- `plot.gg_variable` returns single `ggplot` when `length(xvar)==1`, list otherwise
- `covr::package_coverage` respects `skip_on_cran()` — avoid it for local coverage

## Pre-existing Source Deprecations (warnings, not errors)
- `gg_partial_rfsrc.R:73` — use `all_of(xvar2.name)` instead of bare `xvar2.name`
- `gg_partial_rfsrc.R:103` — use `"type"` instead of `.data$type` in tidyselect
- `plot.gg_variable.R:248` — use `all_of(gathercols)` in `tidyr::gather()`
