# ggRandomForests v2.8.0 — varPro Phase 1 Design
## `gg_partial_varpro`: Partial Dependence for varPro Objects

- **Date:** 2026-05-20
- **Status:** Approved design (pre-implementation)
- **Version bump:** `2.7.3.9001 → 2.7.3.9002`
- **Branch:** `feat/varpro-phase1-gg-partial-varpro`
- **Author:** John Ehrlinger (design partner: Claude)
- **Vault mirror:** `~/Documents/ObsidianVault/R packages/ggRandomForests varPro Integration Design (2026-05-18).md` §5

---

## 1. Scope

Phase 1 of the v2.8.0 varPro integration cycle.  Prerequisite: Phase 0
(`f3ffd38`) and RF-validation (`bd5ebbb`) are both merged to `main`.

**This phase delivers:**
- Rename `gg_partialpro` → `gg_partial_varpro` with a soft-deprecation shim
  retained for ≥1 CRAN cycle.
- Extended signature: optional `object` (originating `varpro` fit) + `scale`
  arg for honest y-axis labeling.
- Survival **path C**: `scale ∈ {surv, chf}` routes `object$rf` through
  `surv_partial.rfsrc()` to produce true S(t)/CHF partial curves within the
  same `gg_partial_varpro` family.
- Dedicated `R/gg_partial_varpro.R` and `R/plot.gg_partial_varpro.R` files.
- varPro promoted from `Suggests:` → `Imports:` (first `varPro::` call lands
  here, as required by the Phase 0 plan).
- Roxygen2 `@details` documenting the "ensemble mortality" definition.

**Out of scope for this phase:** `gg_varpro` (Phase 2), `gg_udependent`
(Phase 3), any model-fitting, `ivarpro`/`isopro`/`beta.varpro` wrappers.

---

## 2. Resolved Implementation Details

### 2a. RMST horizon τ not stored in varPro 3.1.0

Confirmed by inspection: neither the `varpro` fit object nor the `partialpro`
output stores the RMST horizon τ.  Both fitted with and without `rmst=τ` have
identical object structure (`names: rf, split.weight, ..., family`; `family =
"surv"` in both cases).

**Resolution:** τ cannot be auto-detected.  `scale = "auto"` with a survival
`object` defaults to **mortality** labeling.  `scale = "rmst"` requires an
explicit `time = τ` argument; if `time` is `NULL` when `scale = "rmst"`, the
extractor stops with a clear error.

### 2b. C-path via thin adapter to `gg_partial_rfsrc`

Confirmed: `object$rf` in a varPro survival fit is `class c("rfsrc","grow","surv")`
— a standard rfsrc survival object that `surv_partial.rfsrc()` accepts without
modification.

**Resolution:** `scale ∈ {surv, chf}` is implemented as a thin adapter:
```r
# inside gg_partial_varpro(), path C:
surv_dta <- surv_partial.rfsrc(object$rf, ...)
# wrap and label as gg_partial_varpro
```
No bespoke partial computation.  The `scale ∈ {surv, chf}` modes require
`object` to be supplied; clear error otherwise.

---

## 3. Architecture

### 3a. Extractor signature

```r
gg_partial_varpro(
  part_dta  = NULL,
  object    = NULL,
  scale     = c("auto", "rmst", "mortality", "surv", "chf"),
  time      = NULL,
  nvars     = NULL,
  cat_limit = 10,
  model     = NULL
)
```

**Dispatch rules (at least one of `part_dta`, `object` must be supplied):**

| `part_dta` | `object` | `scale` | Behaviour |
|------------|----------|---------|-----------|
| supplied   | NULL     | any     | A-path; generic label + warning if scale cannot be determined |
| NULL       | supplied | auto/mortality/rmst | A-path via `partialpro(object)`; label from `object$family` + `scale` |
| NULL       | supplied | surv/chf | C-path via `surv_partial.rfsrc(object$rf, ...)` |
| both       | both     | any     | `object` supplies provenance/scale; `part_dta` supplies the curves |
| NULL       | NULL     | —       | `stop("at least one of 'part_dta' or 'object' must be supplied")` |
| any        | any      | surv/chf, no `object` | `stop("scale = 'surv'/'chf' requires 'object' (the varpro fit)")` |
| scale=rmst | any      | rmst, `time=NULL` | `stop("scale = 'rmst' requires 'time' (the RMST horizon τ)")` |

### 3b. S3 class

The extractor sets `class(result) <- c("gg_partial_varpro", "list")` on
A-path output (same structure as the old `gg_partialpro`).  C-path output uses the same class, `c("gg_partial_varpro", "list")`.
The `path = "C"` entry in the `provenance` attribute (§3c) distinguishes it
from A-path output; `plot.gg_partial_varpro` reads this to switch rendering.

### 3c. Provenance attribute

Every returned object carries:
```r
attr(result, "provenance") <- list(
  family    = object$family,   # "regr", "class", "surv", or NA
  scale     = scale_used,      # one of the resolved scale strings
  rmst_tau  = time,            # numeric or NA
  xvar.names = object$xvar.names,
  n         = nrow(object$x),
  path      = "A" or "C"
)
```

### 3d. Honest mortality definition — required roxygen block

Every function that can display the mortality scale (`gg_partial_varpro`,
`plot.gg_partial_varpro`) **must** include the following `@details` paragraph
(verbatim or equivalent):

```
@details
**Ensemble mortality (scale = "mortality"):** When displaying the
\code{mortality} scale, the y-axis represents \emph{ensemble mortality}:
the expected number of events if the subject experienced the
study-average cumulative hazard, equivalent to the \code{rfsrc}
\code{predicted} value for survival forests (Ishwaran, Kogalur,
Blackstone & Lauer, 2008 <doi:10.1214/08-AOAS169>).
This is an \strong{unbounded relative-risk score}—\emph{not} a survival
probability or 1 − S(t)—and must not be interpreted as one.  For
probability-scale output, refit with \code{varpro(\ldots, rmst = \tau)}
and use \code{scale = "rmst"}.
```

This block appears in both the extractor and the plot method.

### 3e. Y-axis label map

| `scale` value | Y-axis label |
|---------------|--------------|
| `"mortality"` | `"Ensemble mortality (expected events)"` |
| `"rmst"`      | `"RMST (τ = {time} {time_unit})"` |
| `"surv"`      | `"Survival probability at t = {time}"` |
| `"chf"`       | `"Cumulative hazard at t = {time}"` |
| `"auto"` + regr/class | `"Partial Effect"` (unchanged) |
| `"auto"` + surv, no `object` | `"Partial Effect"` + one-time `message()` |
| `"auto"` + surv, with `object` | `"Ensemble mortality (expected events)"` |

---

## 4. File Changes

### New files

| File | Purpose |
|------|---------|
| `R/gg_partial_varpro.R` | Main extractor (A-path + C-path dispatch) |
| `R/plot.gg_partial_varpro.R` | `plot.gg_partial_varpro` (extracted + extended) |
| `tests/testthat/test_gg_partial_varpro.R` | Full test suite for new family |

### Modified files

| File | Change |
|------|--------|
| `R/gg_partialpro.R` | Replace body with `.Deprecated("gg_partial_varpro")` shim |
| `R/plot.gg_partial.R` | Remove `plot.gg_partialpro` body; replace with shim to `plot.gg_partial_varpro` |
| `R/autoplot_methods.R` | Add `autoplot.gg_partial_varpro`; shim `autoplot.gg_partialpro` |
| `R/print_methods.R` | Add `print.gg_partial_varpro`; shim `print.gg_partialpro` |
| `R/summary_methods.R` | Add `summary.gg_partial_varpro`; shim `summary.gg_partialpro` |
| `DESCRIPTION` | `varPro`: `Suggests:` → `Imports:`; version `2.7.3.9002` |
| `NAMESPACE` | Re-roxygenize: new S3 registrations, `importFrom(varPro,partialpro)` |
| `_pkgdown.yml` | Add `gg_partial_varpro`, `plot.gg_partial_varpro` under Partial Dependence |
| `NEWS.md` | Add `## ggRandomForests 2.7.3.9002` entry |
| `tests/testthat/test_gg_partialpro.R` | Add regression guard: shim emits deprecation warning, output is correct |

---

## 5. Deprecation Shim Pattern

### `R/gg_partialpro.R` (shim)

```r
#' @rdname gg_partial_varpro
#' @export
gg_partialpro <- function(part_dta, nvars = NULL, cat_limit = 10, model = NULL) {
  .Deprecated("gg_partial_varpro",
    msg = paste("'gg_partialpro()' is deprecated.",
                "Use 'gg_partial_varpro()' instead."))
  gg_partial_varpro(part_dta = part_dta, nvars = nvars,
                    cat_limit = cat_limit, model = model)
}
```

S3 methods for the old class are retained as shims that call the new methods.
Old-class objects created before the upgrade (e.g. saved RDS) are handled by
registering `plot.gg_partialpro` etc. as delegating wrappers — they are
**not** removed until the CRAN release after 2.8.0.

---

## 6. `@importFrom` Requirements

```r
#' @importFrom varPro partialpro
```

Added to `R/gg_partial_varpro.R` (the first file that calls `varPro::partialpro()`
when `object` is supplied and `part_dta` is NULL).  This is the line that
triggers the `Suggests: → Imports:` promotion (per Phase 0 plan §locked
decisions).

---

## 7. Tests

### `test_gg_partial_varpro.R` — scope

- Input validation: `stop()` on neither arg, `stop()` on `scale="surv"` without `object`, `stop()` on `scale="rmst"` without `time`
- A-path (mock `partialpro` data, no live varPro): extractor returns correct class, provenance attr, continuous/categorical split
- `part_dta` only: generic label + `message()` warning
- Scale labels: each scale value produces the correct y-axis label
- C-path: `scale="surv"` with a mock rfsrc survival object routes through `surv_partial.rfsrc` (skip_if_not_installed("randomForestSRC") guard)
- `plot.gg_partial_varpro`: continuous-only, categorical-only, both, `type=` arg
- `autoplot`, `print`, `summary`: smoke tests
- Provenance: `attr(result, "provenance")` has expected fields

### `test_gg_partialpro.R` additions

```r
test_that("gg_partialpro shim emits deprecation warning", {
  expect_warning(gg_partialpro(mock_dta),
                 regexp = "deprecated.*gg_partial_varpro",
                 ignore.case = TRUE)
})

test_that("gg_partialpro shim output is a gg_partial_varpro object", {
  result <- suppressWarnings(gg_partialpro(mock_dta))
  expect_s3_class(result, "gg_partial_varpro")
})
```

---

## 8. `_pkgdown.yml` update

The current `"Partial Dependence"` section in `_pkgdown.yml` ends with:

```yaml
      - gg_partialpro
      - plot.gg_partialpro
```

Replace with:

```yaml
      - gg_partial_varpro
      - plot.gg_partial_varpro
      - gg_partialpro          # deprecated alias — keep until removal
      - plot.gg_partialpro     # deprecated alias — keep until removal
```

The deprecated-alias entries keep the old pkgdown pages alive until removal.

---

## 9. NEWS entry

```md
## ggRandomForests 2.7.3.9002

### New features

- `gg_partial_varpro()` replaces `gg_partialpro()` as the primary entry point
  for varPro partial dependence plots (#84).  The new function accepts an
  optional `object` argument (the originating `varpro` fit) for provenance-
  aware axis labeling, and adds a `scale` argument supporting `"auto"`,
  `"mortality"`, `"rmst"`, `"surv"`, and `"chf"`.

- Survival path C: `scale = "surv"` or `scale = "chf"` extracts `object$rf`
  (the embedded rfsrc survival forest) and produces true S(t)/CHF partial
  dependence curves via `surv_partial.rfsrc()`.

- `varPro` is now a hard dependency (`Imports:`) — the first `varPro::` call
  lands in `gg_partial_varpro()`.

### Deprecated

- `gg_partialpro()` is soft-deprecated in favour of `gg_partial_varpro()`.
  It will emit a deprecation warning and delegate to the new function.
  Removal is planned for the release after v2.8.0.
```

---

## 10. Acceptance Criteria

- [ ] `R CMD check --as-cran`: 0 errors, 0 warnings, 0 notes
- [ ] `devtools::test()`: all tests pass; deprecation shim tests pass
- [ ] `lintr::lint_package()`: 0 lints (cyclocomp ≤ 20, line_length ≤ 120)
- [ ] `vdiffr` snapshots generated for all `plot.gg_partial_varpro` modes
- [ ] `gg_partialpro()` emits `warning()` and returns a `gg_partial_varpro` object
- [ ] Mortality `@details` block present in extractor and plot method roxygen
- [ ] `varPro` in `Imports:` in DESCRIPTION; `importFrom(varPro, partialpro)` in NAMESPACE
- [ ] `NEWS.md` header matches `DESCRIPTION` `Version:` field exactly
