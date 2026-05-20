# ggRandomForests v2.8.0 — varPro Phase 2 Design
## `gg_varpro`: Variable Importance for varPro Objects

- **Date:** 2026-05-20
- **Status:** Approved design (pre-implementation)
- **Version bump:** `2.7.3.9002 → 2.7.3.9003`
- **Branch:** `feat/varpro-phase2-gg-varpro` (to be created)
- **Author:** John Ehrlinger (design partner: Claude)
- **Vault mirror:** `~/Documents/ObsidianVault/R packages/ggRandomForests varPro Integration Design (2026-05-18).md` §6

---

## 1. Scope

Phase 2 of the v2.8.0 varPro integration cycle. Prerequisite: Phase 1
(`feat/varpro-phase1-gg-partial-varpro`, PR #84) merged to `main`.

**This phase delivers:**
- `gg_varpro()`: new extractor for varPro variable importance
- `plot.gg_varpro()`: honest 15th/85th boxplot with optional `faithful=TRUE`
  per-tree jitter overlay and `conditional=TRUE` class-faceted rendering
- `autoplot`, `print`, `summary` S3 companions
- Version bump to `2.7.3.9003`
- No deprecation shim needed — `gg_vimp` handles `rfsrc`; `gg_varpro` handles
  `varpro`; they are parallel, not overlapping

**Out of scope for this phase:** `gg_udependent` (Phase 3), any model-fitting,
`ivarpro`/`isopro`/`beta.varpro` wrappers.

---

## 2. Architecture

### Approach: Direct importance wrapper (Approach 1)

`gg_varpro()` calls `importance.varpro()` internally, tidies the result, and
stores everything needed for rendering. The plot method is pure rendering with
no computation. This directly mirrors the Phase 1 extractor/plot pattern.

---

## 3. Extractor Signature

```r
gg_varpro(
  object,             # varpro fit (required)
  local.std = TRUE,   # passed to importance.varpro(); TRUE=z only, FALSE=mean/std/z
  cutoff    = 0.79,   # z threshold; variables below shown as "not selected"
  faithful  = FALSE,  # if TRUE, store imp.tree for per-tree jitter overlay
  conditional = FALSE,# if TRUE, extract $conditional.z (classification only)
  nvar      = NULL,   # top-N variables to retain after cutoff filter; NULL=all
  ...                 # passed to importance.varpro()
)
```

### Dispatch / validation rules

| Condition | Behaviour |
|-----------|-----------|
| `object` missing | `stop("'object' must be a varpro fit")` |
| `conditional=TRUE` + non-classification family | `stop("conditional=TRUE requires a classification forest")` |
| `faithful=TRUE` + `local.std=TRUE` | Force `local.std=FALSE` with `message()`; `mean` is needed for the mean-dot glyph |
| `nvar` supplied | After cutoff filter, retain top-`nvar` by median z |

### Return value

`structure(list(...), class = c("gg_varpro", "list"))` with:

| Slot | Contents |
|------|----------|
| `$imp` | Long tidy data frame: `variable` (factor, ordered by median z desc), `z`, `selected` (logical, `z > cutoff`) |
| `$imp.tree` | Wide per-tree matrix (`ntree × p`) from `importance.varpro(local.std=FALSE)$imp.tree`; `NULL` when `faithful=FALSE` |
| `$stats` | Per-variable summary data frame: `variable`, `median`, `q05`, `q15`, `q85`, `q95`, `mean` (mean only present when `local.std=FALSE`) |
| `$conditional` | `NULL` or long data frame: `variable`, `class`, `z` (when `conditional=TRUE`) |

### Provenance attribute

```r
attr(result, "provenance") <- list(
  family      = object$family,    # "regr", "class", or "surv"
  local.std   = local.std,        # as resolved (may differ from arg if forced)
  cutoff      = cutoff,
  faithful    = faithful,
  conditional = conditional,
  xvar.names  = object$xvar.names,
  n           = nrow(object$x)
)
```

### Internal helpers (cyclocomp ≤ 20)

- `.validate_varpro_imp_inputs(object, local.std, faithful, conditional)` —
  all stop/message logic
- `.varpro_imp_stats(imp_tree_mat)` — compute median/q05/q15/q85/q95/mean
  from the per-tree matrix
- `.build_varpro_imp_dfs(imp_out, cutoff, nvar, faithful)` — tidy `$imp`,
  `$imp.tree`, `$stats`, `$conditional`

---

## 4. Plot Method

```r
plot.gg_varpro(x, type = c("z", "raw"), ...)
```

`type = "z"` (default): x-axis is standardized z-score.
`type = "raw"`: x-axis is raw importance mean. Stops with a clear error if
`local.std=TRUE` was used at extract time (mean not stored).

### Default rendering (`faithful=FALSE`, `conditional=FALSE`)

- Variables on y-axis (`fct_reorder(variable, median)` descending)
- `stat_summary(fun.data = ...)` using pre-computed `$stats` to draw honest
  box: hinges at 15th/85th, whiskers at 5th/95th — bypasses ggplot2's Tukey
  default
- `fill = selected`: above-cutoff variables colored (`#4e8fcd`), below-cutoff
  grey (`#888888`)
- Vertical dashed red line at `cutoff` via `geom_vline(xintercept=cutoff)`
- Mandatory caption: *"Hinges: 15th/85th percentiles; whiskers: 5th/95th.
  Not a Tukey boxplot."*
- X-axis label: `"Variable importance (z)"` or `"Variable importance (mean)"`
  for `type="raw"`

### `faithful=TRUE` rendering

- Same honest box at `alpha=0.4`
- `geom_jitter(data=imp_tree_long, height=0.15, alpha=0.5)` of per-tree z
  (reshape `$imp.tree` to long inside plot method)
- `geom_point(aes(x=mean), shape=21, fill="white", size=2.5)` for mean dot
- Caption: *"Points show per-tree importance. Hinges: 15th/85th; whiskers:
  5th/95th."*

### `conditional=TRUE` rendering

- Uses `$conditional` long df
- `facet_wrap(~class, nrow=1)` — side-by-side panels, one per class
- Shared y-axis (variable names); variable sort from unconditional `$stats$median`
- Cutoff line per panel
- `conditional=TRUE` + `faithful=TRUE`: faithful overlay applied within each facet

### Y-axis label helper

`.varpro_imp_ylabel(type)`:

| `type` | Label |
|--------|-------|
| `"z"` | `"Variable importance (z)"` |
| `"raw"` | `"Variable importance (mean)"` |

---

## 5. `@importFrom` Requirements

```r
#' @importFrom varPro importance
```

Added to `R/gg_varpro.R`. `varPro` is already in `Imports:` from Phase 1.

---

## 6. S3 Companion Methods

### `print.gg_varpro`

Reports: n variables selected / total, family, cutoff, faithful flag.

```
gg_varpro object  [family: surv | cutoff: 0.79 | faithful: FALSE]
  5 of 8 variables selected (z > 0.79)
```

### `summary.gg_varpro`

Prints a table of `variable`, `median z`, `q15`, `q85`, `selected`.

### `autoplot.gg_varpro`

Delegates to `plot(object, ...)`.

---

## 7. File Changes

### New files

| File | Purpose |
|------|---------|
| `R/gg_varpro.R` | Extractor + 3 internal helpers |
| `R/plot.gg_varpro.R` | `plot.gg_varpro` + `.varpro_imp_ylabel` |
| `tests/testthat/test_gg_varpro.R` | Full TDD suite |

### Modified files

| File | Change |
|------|--------|
| `R/autoplot_methods.R` | Add `autoplot.gg_varpro` |
| `R/print_methods.R` | Add `print.gg_varpro` |
| `R/summary_methods.R` | Add `summary.gg_varpro` |
| `DESCRIPTION` | Version `2.7.3.9003`; Date `2026-05-20` |
| `NAMESPACE` | Re-roxygenize: new S3 registrations, `importFrom(varPro,importance)` |
| `_pkgdown.yml` | Add `gg_varpro`, `plot.gg_varpro` under Variable Importance section |
| `NEWS.md` | Add `## ggRandomForests 2.7.3.9003` entry |

---

## 8. Tests (`test_gg_varpro.R`)

### Mock helper

```r
make_mock_varpro <- function(n_obs = 40, n_vars = 5, ntree = 50) {
  set.seed(42)
  # Minimal varpro-like object with $results, $x, $y, $family,
  # $xvar.names, $xvar.org.names, $max.tree, $rf, $split.weight
  # Uses randomForestSRC::rfsrc on iris subset for $rf slot;
  # synthetic $results data.frame with cols: tree, branch, variable, n.oob, imp
}
```

### Test scope

**Input validation:**
- `stop()` when `object` missing
- `stop()` when `conditional=TRUE` on survival/regression family
- `message()` + `local.std` coercion when `faithful=TRUE` + `local.std=TRUE`
- `stop()` for `type="raw"` when `local.std=TRUE` used at extract time

**Class & structure:**
- Returns `"gg_varpro"` class
- `$imp` has columns `variable`, `z`, `selected`
- `$stats` has columns `variable`, `median`, `q05`, `q15`, `q85`, `q95`
- `$imp.tree` is `NULL` when `faithful=FALSE`; matrix when `faithful=TRUE`
- `$conditional` is `NULL` when `conditional=FALSE`; data frame with `variable`, `class`, `z` when `TRUE`
- Provenance attribute has expected fields

**Cutoff & nvar:**
- `selected` column correctly reflects `z > cutoff`
- `nvar=3` retains exactly 3 variables

**Plot smoke tests:**
- `plot(gg_varpro(obj))` returns a `ggplot` object
- `plot(gg_varpro(obj, faithful=TRUE))` returns a `ggplot` object
- `plot(gg_varpro(obj, conditional=TRUE))` returns a `ggplot` with `FacetWrap`
- `plot(..., type="raw")` stops when `local.std=TRUE`

**S3 companions:**
- `autoplot`, `print`, `summary` smoke tests

**vdiffr snapshots** (inside `VDIFFR_RUN_TESTS=true` guard):
- `"gg-varpro-default"` — default regression/classification
- `"gg-varpro-faithful"` — faithful=TRUE
- `"gg-varpro-conditional"` — conditional=TRUE

---

## 9. NEWS Entry

```md
## ggRandomForests 2.7.3.9003

### New features

- `gg_varpro()` extracts and plots variable importance from `varpro` fits (#85).
  Displays an honest boxplot (hinges = 15th/85th percentile, whiskers =
  5th/95th) of per-tree importance z-scores across variables.

- `faithful = TRUE`: overlays individual per-tree z-scores as jittered points
  with a mean dot glyph, reproducing the distributional view from varPro's
  internal `bxp` output.

- `conditional = TRUE`: for classification forests, facets the plot by class
  using the `$conditional.z` matrix from `importance.varpro()`.
```

---

## 10. Acceptance Criteria

- [ ] `R CMD check --as-cran`: 0 errors, 0 warnings, 0 notes
- [ ] `devtools::test()`: all tests pass
- [ ] `lintr::lint_package()`: 0 lints (cyclocomp ≤ 20, line_length ≤ 120)
- [ ] `vdiffr` snapshots generated for default, faithful, conditional modes
- [ ] `faithful=TRUE` plot caption present and correct
- [ ] Provenance attribute present with all expected fields
- [ ] `NEWS.md` header matches `DESCRIPTION` `Version:` exactly
- [ ] `importFrom(varPro, importance)` in NAMESPACE
