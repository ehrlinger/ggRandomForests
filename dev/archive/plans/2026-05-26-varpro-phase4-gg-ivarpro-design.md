# ggRandomForests v2.8.0 — varPro Phase 4d: `gg_ivarpro` Design

**Date:** 2026-05-26
**Author:** John Ehrlinger (design via Claude brainstorming)
**Status:** Approved — ready for implementation planning
**Sequencing:** Fourth and last planned Phase 4 sub-project. Lands as one PR. Followed by the **Phase 5 varPro vignette** (new — consolidates everything learned from Phase 1–4d into a single voice-aligned vignette; replaces the previous looser "vignette voice-audit pass" gate item with a concrete deliverable), then the CRAN audit cleanup PR (`\dontrun` → `\donttest` + chatty message), then the factor-level-alignment follow-up, then v2.8.0 release candidate.

---

## Goal

Give users a tidy wrapper and per-variable-distribution / per-observation-profile plots for `varPro::ivarpro()` — varPro's *individual* (local) variable importance — with the same ergonomics as the rest of Phase 4. Regression and classification supported in this PR.

## Scope

Single PR. `regr` and `class` (binary + multi-class) families. `regr+` and `surv` remain deferred.

---

## What `ivarpro` actually returns

Empirically verified against `varPro 3.1.0`:

| Family | Top-level shape |
|---|---|
| `regr` | `data.frame`, `nrow = n_train`, `ncol = p` (all predictors via `xvar.org.names`). Cells = local importance per (obs, var). Heavily sparse: per-variable NA fractions ranged 25–100% on Boston housing. |
| `class` (binary + multi-class) | **Named list of K data.frames**, names = response factor levels, each `n_train × p`. Same sparsity profile per class. |

Attributes carried on the returned object (same shape for both families):

- `data` — training data with response appended as `y`
- `model` — the source `varpro` fit
- `ivarpro.path` — per-rule scaffolding (`rule.variable`, `rule.imp`, `rule.imp.ladder`, `rule.slope`, `rule.scale`, `rule.tree`, `rule.branch`, `oobMembership`, `compMembership`)

`ivarpro` signature (forwardable via `…`):
```
ivarpro(object, adaptive = TRUE, cut = NULL, cut.max = 1, ncut = 51,
        nmin = 20, nmax = 150, y.external = NULL, noise.na = TRUE,
        max.rules.tree = NULL, max.tree = NULL, use.loo = TRUE,
        use.abs = FALSE, path.store.membership = TRUE,
        save.data = TRUE, save.model = TRUE,
        scale = c("local", "global", "none"))
```

`ivarpro` is **the most expensive call in varPro** (per-rule LOO + optional ladder + per-region scaling). Caching is non-optional in the design.

---

## Architecture

```
varPro::varpro fit (regr or class)  ──►  gg_ivarpro(fit, ...,
                                                    which_obs = NULL,
                                                    which_class = NULL,
                                                    cutoff = NULL,
                                                    ivarpro_fit = NULL)
                                                    │
                                                    ├── family dispatch
                                                    │     ├─ regr  → tidy (obs, variable, local_imp)
                                                    │     └─ class → tidy (obs, variable, class, local_imp)
                                                    │
                                                    └── data.frame, class = c("gg_ivarpro", "data.frame")
                                                        attr: provenance, ivarpro (source object), data
                                                              │
                                                        plot / print / summary / autoplot
```

Single S3 class on the returned frame. `plot.gg_ivarpro()` branches on:
- presence of `class` column (classification vs regression)
- `which_obs` provenance (per-observation profile vs distribution)
- number of unique classes after filtering (single panel vs facet)

---

## Signature

```r
gg_ivarpro(object, ..., which_obs = NULL, which_class = NULL,
           cutoff = NULL, ivarpro_fit = NULL)
```

All trailing args named-only (Phase 4c convention, locks in back-compat for any future positional argument).

- **`object`** — `varpro` fit (`regr` or `class`). Other families error with the deferred-work signpost.
- **`…`** — forwarded to `varPro::ivarpro()` when `ivarpro_fit = NULL`. Documented forwardables: `adaptive`, `cut`, `cut.max`, `ncut`, `nmin`, `nmax`, `noise.na`, `max.rules.tree`, `max.tree`, `use.loo`, `use.abs`, `scale`. (Not forwarded: `y.external`, `path.store.membership`, `save.data`, `save.model` — see Out of scope.)
- **`which_obs`** — integer scalar 1-based row index into `attr(., "data")` (the training rows ivarpro saw). `NULL` (default) = aggregate view. Out-of-range integer errors with the valid range listed.
- **`which_class`** — classification only. `NULL` default: binary → resolves to last factor level (positive class); multi-class → `NULL` stays. String = single class. Not-in-levels error lists the valid levels. Supplied on a regression fit → warning, ignored.
- **`cutoff`** — selection threshold. Same polymorphism as gg_beta_varpro classification:
  - `NULL` → per-class (or per-regression-frame) `mean(|local_imp|)`
  - scalar → broadcast across classes
  - named numeric vector (names ⊆ class levels) → per-class override with fallback to per-class mean for missing names
- **`ivarpro_fit`** — pre-computed `ivarpro()` result. Shape-validated (see Errors). `…` warns when supplied alongside.

---

## Tidy frame schemas

**Regression** (`regr`):

| Column | Type | Meaning |
|---|---|---|
| `obs` | integer | 1-based row index into `attr(., "data")` |
| `variable` | factor | Predictor name. Levels set by `mean(|local_imp|)` descending across all obs. |
| `local_imp` | numeric | Local importance for (obs, variable). NA cells filtered out. |
| `selected` | logical | `abs(local_imp) >= cutoff` |

**Classification** (`class`, binary + multi-class):

| Column | Type | Meaning |
|---|---|---|
| `obs` | integer | as above |
| `variable` | factor | Levels set by `mean(|local_imp|)` descending across **all obs and all classes** (the unified "total importance" axis; same convention as gg_beta_varpro classification) |
| `class` | factor | Levels = response factor levels in order |
| `local_imp` | numeric | Local importance for (obs, variable, class). NA cells filtered out. |
| `selected` | logical | `abs(local_imp) >= cutoff[[class]]` |

**Sort order in the frame:** classification rows sorted by `class` (factor level), then `variable` (factor level), then `obs`. Regression: `variable`, then `obs`.

---

## Internal flow

1. Validate `inherits(object, "varpro")`, `object$family %in% c("regr", "class")`. Anything else errors with the deferred-work signpost.
2. Capture `dots_use_loo <- if (is.null(ivarpro_fit)) isTRUE(list(...)$use.loo) else NA` (and same idiom for `scale`) before dispatch — same closure-scope discipline as Phase 4c.
3. Resolve `ivarpro_fit`:
   - `NULL` → run `iv <- varPro::ivarpro(object, ...)` (expensive)
   - Non-NULL → validate via `.validate_ivarpro_fit(ivarpro_fit, object, fam)` (see Errors); skip the expensive call; warn on non-empty `…`
4. Warn on `which_class` with regression; warn on `which_obs` not in `[1, n_train]`.
5. Family dispatch to `.gg_ivarpro_regr()` or `.gg_ivarpro_class()`:
   - Coerce source matrix/list-of-matrices to long format, filtering NA cells.
   - Compute total |local_imp| per variable (unified ranking score) → set factor levels.
   - For classification: compute per-class `beta_mean`-equivalent (`mean(|local_imp|)`) per class for the cutoff resolution.
   - Resolve `cutoff` polymorphism (NULL → per-class mean; scalar → broadcast; named vector → per-class with fallback).
   - Apply `which_obs` / `which_class` filters last.
6. Build provenance, set class, return.

---

## Errors

- Non-`varpro` input → `"gg_ivarpro: expected a 'varpro' object from varPro::varpro()."`
- Non-regression/classification family → `"gg_ivarpro currently supports varpro regression and classification forests only; got family = '<x>'. regr+ and survival are tracked under Phase 4d follow-ups (see NEWS)."`
- `ivarpro_fit` malformed → `.validate_ivarpro_fit` raises:
  - regression: must be a `data.frame` with the right `xvar.org.names` columns and `n_train` rows
  - classification: must be a list of `length == K` named with the class levels, each element a `data.frame` of shape `n_train × p`
- `which_obs` not in `1:n_train` → `"which_obs = <i> is out of range. Valid range: 1..<n_train>."`
- `which_class` not in levels → `"which_class = '<x>' is not a level of the response. Levels: <a, b, ...>."`
- `…` non-empty alongside `ivarpro_fit` → `warning("arguments in '...' ignored because ivarpro_fit is supplied.")`
- Empty source (no non-NA cells anywhere) → empty tidy frame, provenance `n_obs = 0`; downstream `plot()` errors with a non-cryptic message.

---

## Plot

`plot.gg_ivarpro(x, ...)` returns a single `ggplot`. Dispatch matrix:

| `which_obs` provenance | `class` column in `x` | Layer |
|---|---|---|
| `NULL` | absent (regr) | `geom_jitter(width = 0.2, height = 0, alpha = 0.5)` of `(variable, local_imp)`; one point per obs |
| `NULL` | present (cls) | Same, `facet_wrap(~ class)` (or single facet when `which_class` set) |
| integer | absent (regr) | `geom_col()` of one bar per variable for that obs; `coord_flip()` |
| integer | present (cls) | Same, `facet_wrap(~ class)` (or single facet) |

Shared:
- `coord_flip()` (always — variable on y).
- Fill / colour by `selected` (blue `#4e8fcd` / grey `#888888`, matching gg_beta_varpro).
- Per-class cutoff line via `geom_hline(data = hline_df, aes(yintercept = cutoff), inherit.aes = FALSE)` when faceting; single `geom_hline(yintercept = scalar)` otherwise.
- Caption: `"Local importance over <n_obs> observations × <n_var> variables. NA fraction: <pct>. <Class summary>."` with `<Class summary>` empty / single-class / "K classes (faceted)" depending on mode.

---

## S3 companions (placement matches Phase 4c precedent)

- `print.gg_ivarpro` lives in `R/print_methods.R`, uses `.gg_header()`. Branches on `class` column + `which_obs` provenance to surface mode succinctly.
- `summary.gg_ivarpro` lives in `R/summary_methods.R`. Returns named numeric (regression) or list of per-class named-numeric (classification) of `mean(|local_imp|)` per variable, each with `n_obs` (count of contributing observations) as an attribute.
- `autoplot.gg_ivarpro` lives in `R/gg_ivarpro.R`; delegates to `plot.gg_ivarpro`.

---

## Provenance attribute

```r
list(
  source           = "varPro::ivarpro",
  family           = "regr" | "class",
  ntree            = object$ntree,
  cutoff           = named numeric (length 1 "regr" or length K),
  cutoff_default   = is.null(cutoff),
  use.loo          = passed-or-NA,
  scale            = passed-or-NA,
  n_train          = n,
  n_obs            = nrow(unique(out$obs)),
  n_var            = nlevels(out$variable),
  na_fraction      = named numeric per variable (regression) or per (class, variable) (class),
  precomputed      = !is.null(ivarpro_fit),
  xvar.names       = full predictor names (xvar.org.names),
  class_levels     = K-vector (class only),
  which_obs        = integer or NULL,
  which_class      = string or NULL
)
```

`cutoff` is always a named numeric vector (length 1 named `"regr"` for regression; length K named with class levels for classification) — same contract as gg_beta_varpro after PR #98.

---

## Caching

`ivarpro` is the most expensive call in the package. The `ivarpro_fit` arg lets callers compute once and reuse across multiple wrapper calls (different `which_obs`, `which_class`, `cutoff`):

```r
v   <- varPro::varpro(medv ~ ., data = Boston, ntree = 200)
iv  <- varPro::ivarpro(v, scale = "local")              # expensive, once
gg_aggregate <- gg_ivarpro(v, ivarpro_fit = iv)         # cheap
gg_obs1      <- gg_ivarpro(v, ivarpro_fit = iv, which_obs = 1)
gg_high_med  <- gg_ivarpro(v, ivarpro_fit = iv,
                           which_obs = which.max(attr(v, "data")$medv))
```

Provenance carries `precomputed = TRUE` when supplied.

Test fixture pattern continues the Phase 4c approach: rename `tests/testthat/helper-beta_varpro.R` → `tests/testthat/helper-varpro-fixtures.R`; merge `.ivarpro_*` fixtures into the same session-memoised `.varpro_cache` env. Boston (regr) and iris (class binary + multi-class) at fixed seed.

---

## Tests (~18)

1. Regression shape: long-format frame, columns `obs / variable / local_imp / selected`.
2. Classification shape: extra `class` column, K class levels present.
3. NA cells filtered out: `nrow(out)` equals count of non-NA cells in the source.
4. `variable` is a factor ordered by descending `mean(|local_imp|)` across all rows (regression) or across all (obs, class) (classification).
5. `which_obs = i` returns only `obs == i` rows; provenance `which_obs == i`.
6. `which_obs` out-of-range errors with the valid range listed.
7. `which_obs` on classification keeps the `class` column.
8. `which_class = NULL` on binary resolves to last factor level.
9. `which_class = "x"` returns only that class.
10. `which_class` not in levels errors with levels listed.
11. `which_class` on regression warns and is ignored.
12. `cutoff = NULL` → per-class mean (named numeric vector in provenance).
13. `cutoff` scalar → broadcast across classes.
14. `cutoff` named-vector override → per-class with fallback.
15. `ivarpro_fit` cache equivalence (slow-guarded by `GG_IVARPRO_SLOW_TESTS`, defaulting on locally, off in `_R_CHECK_LIMIT_CORES_`-style CRAN gate).
16. `ivarpro_fit` shape guard — regression: pass a class shape, expect error; classification: pass an incomplete list (missing one class), expect error.
17. `…` + `ivarpro_fit` → single warning.
18. Plot smoke — all four modes (regr aggregate / regr which_obs / cls aggregate / cls which_obs) build via `ggplot_build()`. Print + summary smoke. Autoplot identical to plot.

---

## Snapshots (4 vdiffr baselines under existing `VDIFFR_RUN_TESTS` guard)

- `gg-ivarpro-regr-distribution`
- `gg-ivarpro-regr-which-obs`
- `gg-ivarpro-class-distribution`
- `gg-ivarpro-class-which-obs`

---

## Documentation

- New roxygen pages for `gg_ivarpro` and `plot.gg_ivarpro` (markdown register, PR #95 enabled).
- `@section What this is doing:` — one paragraph on per-rule LOO + per-region scaling.
- ``@section What `local_imp` actually is:`` — pedantic block. Sign convention, scaling (local / global / none and what each means), `use.loo`, `noise.na`, sparsity caveat. Code form: `local_imp[i, v] = scaled per-rule contribution of variable v to predicting observation i, aggregated over rules`.
- `@section What's in the output:` — column descriptions, factor-level convention, NA filtering.
- `@section What you use this for:` — per-observation interpretation (which variables drive *this* prediction), aggregate per-variable distribution for variable selection diagnostics, comparison with `gg_varpro` and `gg_beta_varpro` aggregates.
- `@section Caching:` — `ivarpro_fit` pattern, with the worked example above.
- `@section Classification:` — long-format `class` column, `which_class` semantics, per-class cutoffs, faceted plot.
- `@section Reproducibility:` — cached vs uncached agreement requires `ivarpro_fit` reuse (lasso CV folds + per-region LOO subsampling can drift across calls under just `set.seed()`).

`_pkgdown.yml` — add `gg_ivarpro` and `plot.gg_ivarpro` to the varPro reference group.

---

## Files

- **New**: `R/gg_ivarpro.R`, `R/plot.gg_ivarpro.R`, `tests/testthat/test_gg_ivarpro.R`
- **Renamed**: `tests/testthat/helper-beta_varpro.R` → `tests/testthat/helper-varpro-fixtures.R` (merge `.ivarpro_*` fixtures into same cache env)
- **Modify**: `R/print_methods.R`, `R/summary_methods.R` (append two new methods each); `tests/testthat/test_snapshots.R` (four baselines); `DESCRIPTION` (`2.7.3.9013`); `NEWS.md`; `_pkgdown.yml`
- **New (follow-up sub-projects, not in this PR)**: Phase 5 varPro vignette spec; `\dontrun` / chatty-message cleanup spec; factor-level alignment spec for `gg_vimp` / `plot.gg_varpro(conditional = TRUE)`

---

## Acceptance criteria

- `R CMD check --as-cran`: 0 / 0 / 0
- `lintr::lint_package()`: 0 issues
- `devtools::test()`: 0 failures (CRAN-safe subset)
- `GG_IVARPRO_SLOW_TESTS=true VDIFFR_RUN_TESTS=true devtools::test()`: 0 failures, four snapshots build
- pkgdown reference renders without warnings
- One PR before the Phase 5 vignette PR and the v2.8.0 release-candidate gate

---

## Out of scope (Phase 4d)

- `y.external` (re-scoring with a different response vector) — interesting; v2.9.0 brainstorm
- Cell-by-cell heatmap visualization — different mental model; punt
- `nmin` / `nmax` / `adaptive` tuning hooks beyond `…` forwarding
- `regr+` and `surv` family support (upstream may not even support them in `ivarpro`)
- Row-name-based `which_obs` lookup (integer index only)
- The Phase 5 varPro vignette (separate sub-project, own brainstorm)
- The `\dontrun`/chatty-message cleanup, and the factor-level alignment — each is a separate follow-up sub-project, tracked in the v2.8.0 runway above
