# varPro Partial-Plot Scales (v3.3.0) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `gg_partial_varpro()` return interpretable y-axis scales — probability by default for classification, survival probability S(τ) by default for survival — with odds/log-odds and mortality/RMST as options, while keeping the `causal` contrast honest.

**Architecture:** All changes are in two R files plus tests/docs. The extractor (`gg_partial_varpro` + internals in `R/gg_partial_varpro.R`) resolves a per-family scale, converts the partialpro log-odds matrices to the chosen scale *before* averaging, blanks the `causal` contrast on bounded scales, and adds a survival S(t) learner with a median-follow-up default τ. The plot method (`R/plot.gg_partial_varpro.R`) builds honest y-labels and drops/warns on `causal` for bounded scales.

**Tech Stack:** R, roxygen2, testthat (+ `devtools::load_all`/`document`), `randomForestSRC::predict.rfsrc`, `varPro::partialpro`.

**Design spec:** `dev/plans/2026-06-23-varpro-partial-scales-design.md`

**Branch:** `feat/varpro-figures-3.3.0` (already at version 3.3.0).

---

## File Structure

- `R/gg_partial_varpro.R` — extractor + internals. New helpers: `.scale_transform`, `.is_bounded_scale`, `.surv_at_tau`, `.surv_learner`, `.default_surv_tau`, `.varpro_target`. Modified: `gg_partial_varpro` signature/routing, `.resolve_varpro_scale`, `.build_varpro_dfs`, `.process_cat_var`, `.validate_varpro_inputs`/`.validate_rmst_inputs`, `.varpro_provenance`.
- `R/plot.gg_partial_varpro.R` — `.partial_varpro_ylabel` (new labels), `plot.gg_partial_varpro` (causal drop/warn), roxygen `@section`s/`@references`.
- `R/gg_partialpro.R` — no code change (alias inherits via `...`); roxygen `@references` only.
- `tests/testthat/test_gg_partial_varpro.R` — new tests appended.
- `NEWS.md`, `man/` — version bullets + regenerated docs.

**Test command used throughout** (the repo skips varpro grows on CRAN, so set `NOT_CRAN`):

```bash
NOT_CRAN=true Rscript -e 'suppressMessages(devtools::load_all(quiet=TRUE)); testthat::test_file("tests/testthat/test_gg_partial_varpro.R", reporter="summary")'
```

Single internal helpers are exercised via `ggRandomForests:::name(...)` inside tests (the file already uses this pattern).

---

### Task 1: Scale vocabulary + resolution

Add the classification scale values and change `auto` resolution: classification → `prob`, survival → `surv`.

**Files:**
- Modify: `R/gg_partial_varpro.R` — `gg_partial_varpro` signature `scale = c(...)`, and `.resolve_varpro_scale`.
- Test: `tests/testthat/test_gg_partial_varpro.R`

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test_gg_partial_varpro.R`:

```r
## ── v3.3.0 scale resolution ──────────────────────────────────────────────────
test_that(".resolve_varpro_scale: auto resolves by family (3.3.0)", {
  expect_equal(ggRandomForests:::.resolve_varpro_scale("auto", "class"), "prob")
  expect_equal(ggRandomForests:::.resolve_varpro_scale("auto", "surv"),  "surv")
  expect_equal(ggRandomForests:::.resolve_varpro_scale("auto", "regr"),  "generic")
  expect_equal(ggRandomForests:::.resolve_varpro_scale("auto", NA_character_),
               "generic")
  # explicit scales pass through unchanged
  expect_equal(ggRandomForests:::.resolve_varpro_scale("prob", "class"), "prob")
  expect_equal(ggRandomForests:::.resolve_varpro_scale("odds", "class"), "odds")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run the test command above.
Expected: FAIL — current `.resolve_varpro_scale("auto","class")` returns `"generic"`, `"auto"/"surv"` returns `"mortality"`.

- [ ] **Step 3: Update the `scale` argument and resolver**

In `R/gg_partial_varpro.R`, change the `gg_partial_varpro` signature default for `scale` from:

```r
                               scale     = c("auto", "rmst", "mortality",
                                             "surv", "chf"),
```

to:

```r
                               scale     = c("auto", "prob", "odds", "logodds",
                                             "rmst", "surv", "mortality", "chf"),
```

Replace the body of `.resolve_varpro_scale`:

```r
#' @keywords internal
.resolve_varpro_scale <- function(scale, family) {
  if (scale != "auto") return(scale)
  if (is.na(family) || is.null(family)) return("generic")
  if (family == "surv")  return("surv")    # bounded survival default (3.3.0)
  if (family == "class") return("prob")    # probability default (3.3.0)
  "generic"   # regr or unknown
}
```

- [ ] **Step 4: Run test to verify it passes**

Run the test command. Expected: the new resolution test PASSES. (Other tests may now fail — that is expected and fixed in later tasks; note any failures and continue.)

- [ ] **Step 5: Commit**

```bash
git add R/gg_partial_varpro.R tests/testthat/test_gg_partial_varpro.R
git commit -m "feat(gg_partial_varpro): scale vocabulary + auto->prob/surv resolution (3.3.0)"
```

---

### Task 2: Scale-transform + bounded-scale helpers

Two pure helpers: the value transform (log-odds → prob/odds) and the bounded-scale predicate.

**Files:**
- Modify: `R/gg_partial_varpro.R` (add helpers near the other internals).
- Test: `tests/testthat/test_gg_partial_varpro.R`

- [ ] **Step 1: Write the failing test**

```r
## ── v3.3.0 scale transform + bounded predicate ───────────────────────────────
test_that(".scale_transform applies prob/odds/identity", {
  z <- matrix(c(0, log(3)), nrow = 1)        # log-odds: 0 -> p=.5, log(3) -> p=.75
  expect_equal(ggRandomForests:::.scale_transform(z, "prob"),
               matrix(c(0.5, 0.75), nrow = 1))
  expect_equal(ggRandomForests:::.scale_transform(z, "odds"),
               matrix(c(1, 3), nrow = 1))
  # identity for the additive / non-classification scales
  for (s in c("logodds", "generic", "surv", "rmst", "mortality"))
    expect_equal(ggRandomForests:::.scale_transform(z, s), z)
})

test_that(".is_bounded_scale flags prob/odds/surv only", {
  for (s in c("prob", "odds", "surv"))
    expect_true(ggRandomForests:::.is_bounded_scale(s))
  for (s in c("logodds", "generic", "mortality", "rmst", "chf"))
    expect_false(ggRandomForests:::.is_bounded_scale(s))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run the test command. Expected: FAIL — `.scale_transform`/`.is_bounded_scale` not found.

- [ ] **Step 3: Add the helpers**

In `R/gg_partial_varpro.R`, add near `.resolve_varpro_scale`:

```r
## Transform partialpro's (log-odds) values to the requested classification
## scale. Identity for everything except prob/odds, because the survival
## learners already return their own scale and the additive scales are raw.
#' @keywords internal
.scale_transform <- function(z, scale) {
  switch(scale,
    prob = stats::plogis(z),
    odds = exp(z),
    z)
}

## Bounded scales: probability (class), odds (class), survival probability.
## On these the absolute level curves convert and the centered `causal`
## contrast is not shown (it cannot share the axis).
#' @keywords internal
.is_bounded_scale <- function(scale) {
  scale %in% c("prob", "odds", "surv")
}
```

- [ ] **Step 4: Run test to verify it passes**

Run the test command. Expected: both new tests PASS.

- [ ] **Step 5: Commit**

```bash
git add R/gg_partial_varpro.R tests/testthat/test_gg_partial_varpro.R
git commit -m "feat(gg_partial_varpro): .scale_transform + .is_bounded_scale helpers"
```

---

### Task 3: Convert + blank causal in the data-frame builders

Apply `.scale_transform` to the `par`/`nonpar` matrices **before** `colMeans` (mean of probabilities), and set `causal = NA` on bounded scales. Thread `scale` through `.build_varpro_dfs` and `.process_cat_var`.

**Files:**
- Modify: `R/gg_partial_varpro.R` — `.build_varpro_dfs`, `.process_cat_var`, and the one call site in `gg_partial_varpro`.
- Test: `tests/testthat/test_gg_partial_varpro.R`

- [ ] **Step 1: Write the failing test**

```r
## ── v3.3.0 conversion in the extractor (mean of probabilities) ───────────────
test_that("gg_partial_varpro: scale='prob' is mean of plogis, causal NA", {
  d <- make_mock_vpro_data()
  res <- gg_partial_varpro(d, scale = "prob")     # part_dta path, explicit scale
  age <- res$continuous[res$continuous$name == "age", ]
  expected <- colMeans(stats::plogis(d$age$yhat.par), na.rm = TRUE)
  expect_equal(age$parametric, expected)          # mean of probabilities
  expect_true(all(age$parametric >= 0 & age$parametric <= 1))
  expect_true(all(is.na(res$continuous$causal)))  # causal blanked on bounded
})

test_that("gg_partial_varpro: scale='logodds' keeps raw values + causal", {
  d <- make_mock_vpro_data()
  res <- gg_partial_varpro(d, scale = "logodds")
  age <- res$continuous[res$continuous$name == "age", ]
  expect_equal(age$parametric, colMeans(d$age$yhat.par, na.rm = TRUE))
  expect_false(all(is.na(res$continuous$causal)))  # causal shown on additive
})
```

(`make_mock_vpro_data()` already exists at the top of the test file.)

- [ ] **Step 2: Run test to verify it fails**

Run the test command. Expected: FAIL — `gg_partial_varpro` does not accept/apply the scale to the values yet (`parametric` is raw log-odds, `causal` not NA).

- [ ] **Step 3: Thread `scale` and apply the transform**

In `R/gg_partial_varpro.R`, change the `.build_varpro_dfs` signature and body. Replace:

```r
#' @keywords internal
.build_varpro_dfs <- function(part_dta, nvars, cat_limit) {
  cont_list <- list()
  cat_list  <- list()
  for (feature in seq(nvars)) {
    feat      <- part_dta[[feature]]
    feat_name <- names(part_dta)[[feature]]
    if (length(feat$xvirtual) > cat_limit) {
      plt.df <- dplyr::bind_cols(
        variable      = feat$xvirtual,
        parametric    = colMeans(feat$yhat.par,    na.rm = TRUE),
        nonparametric = colMeans(feat$yhat.nonpar, na.rm = TRUE),
        causal        = colMeans(feat$yhat.causal, na.rm = TRUE)
      )
      plt.df$name <- feat_name
      cont_list[[feature]] <- plt.df
    } else {
      cat_list[[feature]] <- .process_cat_var(feat, feat_name)
    }
  }
  list(
    continuous  = dplyr::bind_rows(cont_list),
    categorical = dplyr::bind_rows(cat_list)
  )
}
```

with:

```r
#' @keywords internal
.build_varpro_dfs <- function(part_dta, nvars, cat_limit, scale = "generic") {
  bounded   <- .is_bounded_scale(scale)
  cont_list <- list()
  cat_list  <- list()
  for (feature in seq(nvars)) {
    feat      <- part_dta[[feature]]
    feat_name <- names(part_dta)[[feature]]
    if (length(feat$xvirtual) > cat_limit) {
      plt.df <- dplyr::bind_cols(
        variable      = feat$xvirtual,
        parametric    = colMeans(.scale_transform(feat$yhat.par,    scale),
                                 na.rm = TRUE),
        nonparametric = colMeans(.scale_transform(feat$yhat.nonpar, scale),
                                 na.rm = TRUE),
        # `causal` is a centered contrast: not shown on bounded scales
        causal        = if (bounded) NA_real_ else
          colMeans(feat$yhat.causal, na.rm = TRUE)
      )
      plt.df$name <- feat_name
      cont_list[[feature]] <- plt.df
    } else {
      cat_list[[feature]] <- .process_cat_var(feat, feat_name, scale)
    }
  }
  list(
    continuous  = dplyr::bind_rows(cont_list),
    categorical = dplyr::bind_rows(cat_list)
  )
}
```

Replace `.process_cat_var`:

```r
#' @keywords internal
.process_cat_var <- function(feat, feat_name) {
  n_cats   <- length(unique(feat$xorg))
  cat_feat <- list()
  for (ind in seq(n_cats)) {
    cat_feat[[ind]] <- dplyr::bind_cols(
      parametric    = feat$yhat.par[, ind],
      nonparametric = feat$yhat.nonpar[, ind],
      causal        = feat$yhat.causal[, ind]
    )
    cat_feat[[ind]]$variable <- unique(feat$xorg)[ind]
    plt.df <- if (ind == 1L) cat_feat[[ind]] else
      dplyr::bind_rows(plt.df, cat_feat[[ind]])
  }
  plt.df$name <- feat_name
  plt.df
}
```

with:

```r
#' @keywords internal
.process_cat_var <- function(feat, feat_name, scale = "generic") {
  bounded  <- .is_bounded_scale(scale)
  n_cats   <- length(unique(feat$xorg))
  cat_feat <- list()
  for (ind in seq(n_cats)) {
    cat_feat[[ind]] <- dplyr::bind_cols(
      parametric    = .scale_transform(feat$yhat.par[, ind],    scale),
      nonparametric = .scale_transform(feat$yhat.nonpar[, ind], scale),
      causal        = if (bounded) NA_real_ else feat$yhat.causal[, ind]
    )
    cat_feat[[ind]]$variable <- unique(feat$xorg)[ind]
    plt.df <- if (ind == 1L) cat_feat[[ind]] else
      dplyr::bind_rows(plt.df, cat_feat[[ind]])
  }
  plt.df$name <- feat_name
  plt.df
}
```

In `gg_partial_varpro`, **resolve `scale` once, early** — immediately after the
input-validation block (after the `...`-length warning, *before* the C-path
block). Validation needs the raw `"auto"`; everything downstream (routing,
conversion, labels, provenance) needs the concrete scale. Insert:

```r
  ## Resolve 'auto' to a concrete scale once; all routing, conversion, labels
  ## and provenance below use the resolved value. (Validation above used the
  ## raw scale, which must still distinguish 'auto'.)
  scale <- .resolve_varpro_scale(
    scale, if (!is.null(object)) object$family else NA_character_)
```

Then change the dfs call site from:

```r
  dfs <- .build_varpro_dfs(part_dta, nvars, cat_limit)
```

to:

```r
  dfs <- .build_varpro_dfs(part_dta, nvars, cat_limit, scale)
```

Because `scale` is now the resolved value, the existing C-path check
(`scale %in% c("surv", "chf")`) still behaves correctly in this task (explicit
or auto-resolved `surv` routes to path C for now); Task 6 narrows it to `chf`
and adds the `surv` learner.

- [ ] **Step 4: Run test to verify it passes**

Run the test command. Expected: the two new conversion tests PASS. The earlier numeric test "continuous parametric equals colMeans(yhat.par)" still passes because the default `part_dta`-only call resolves to `generic` (identity transform).

- [ ] **Step 5: Commit**

```bash
git add R/gg_partial_varpro.R tests/testthat/test_gg_partial_varpro.R
git commit -m "feat(gg_partial_varpro): convert par/nonpar before averaging; blank causal on bounded scales"
```

---

### Task 4: Survival S(t) learner

`.surv_at_tau` pulls the S(τ) column; `.surv_learner` is the partialpro learner (mirrors `.rmst_learner`).

**Files:**
- Modify: `R/gg_partial_varpro.R` (add both helpers near `.rmst_learner`).
- Test: `tests/testthat/test_gg_partial_varpro.R`

- [ ] **Step 1: Write the failing test**

```r
## ── v3.3.0 survival S(t) learner ─────────────────────────────────────────────
test_that(".surv_at_tau pulls S(tau) snapped to nearest event time", {
  surv  <- matrix(c(0.9, 0.6, 0.3), nrow = 1)   # S(1), S(2), S(3)
  times <- c(1, 2, 3)
  expect_equal(ggRandomForests:::.surv_at_tau(surv, times, 2),   0.6)
  expect_equal(ggRandomForests:::.surv_at_tau(surv, times, 2.1), 0.6)  # nearest
  expect_equal(ggRandomForests:::.surv_at_tau(surv, times, 3),   0.3)
})

test_that(".surv_learner returns S(tau) in [0,1] for OOB and newdata", {
  skip_if_not_installed("randomForestSRC")
  m       <- make_mock_cpath()              # defined later in this file
  tau     <- stats::median(m$rf$time.interest)
  learner <- ggRandomForests:::.surv_learner(list(rf = m$rf), tau)
  oob <- learner()                          # missing(newx) -> OOB
  nd  <- learner(survival::veteran[1:5, ])  # newdata branch
  expect_length(oob, nrow(m$rf$xvar))
  expect_length(nd, 5L)
  expect_true(all(oob >= 0 & oob <= 1))
  expect_true(all(nd  >= 0 & nd  <= 1))
})
```

`make_mock_cpath()` is defined lower in the test file (testthat sources the whole file before running, but `test_that` blocks execute top-to-bottom). **Place these two tests AFTER the `make_mock_cpath` definition** (search for `make_mock_cpath <- function` and add them just below it, alongside the existing `.rmst_learner` test).

- [ ] **Step 2: Run test to verify it fails**

Run the test command. Expected: FAIL — `.surv_at_tau`/`.surv_learner` not found.

- [ ] **Step 3: Add the helpers**

In `R/gg_partial_varpro.R`, add directly after `.rmst_learner`/`.rmst_from_survival`:

```r
## Survival probability at horizon tau from a survival matrix: the S(tau)
## column, snapped to the nearest event time. `surv` is n x J with column k =
## S(times[k]) (randomForestSRC layout).
#' @keywords internal
.surv_at_tau <- function(surv, times, tau) {
  if (is.null(dim(surv))) surv <- matrix(surv, nrow = 1L)
  if (ncol(surv) != length(times)) {
    stop(sprintf(paste0(".surv_at_tau: survival matrix has %d column(s) but ",
                        "%d time point(s); the grids must match."),
                 ncol(surv), length(times)), call. = FALSE)
  }
  surv[, which.min(abs(times - tau))]
}

## S(tau) learner for varPro::partialpro: maps feature rows to S(tau | x) from
## the survival forest in object$rf. Same prediction machinery as
## .rmst_learner; pulls the S(tau) column instead of integrating.
#' @keywords internal
.surv_learner <- function(object, tau) {
  rf <- object$rf
  function(newx) {
    if (missing(newx)) {
      pr   <- randomForestSRC::predict.rfsrc(rf, perf.type = "none")
      surv <- pr$survival.oob
      if (is.null(surv)) surv <- pr$survival
    } else {
      pr   <- randomForestSRC::predict.rfsrc(rf, newx, perf.type = "none")
      surv <- pr$survival
    }
    times <- pr$time.interest
    if (is.null(times)) times <- rf$time.interest
    .surv_at_tau(surv, times, tau)
  }
}
```

- [ ] **Step 4: Run test to verify it passes**

Run the test command. Expected: both new survival-learner tests PASS.

- [ ] **Step 5: Commit**

```bash
git add R/gg_partial_varpro.R tests/testthat/test_gg_partial_varpro.R
git commit -m "feat(gg_partial_varpro): .surv_learner / .surv_at_tau (survival probability)"
```

---

### Task 5: Default τ = median follow-up

`.default_surv_tau` returns the median observed survival time, with a fallback to `median(time.interest)`.

**Files:**
- Modify: `R/gg_partial_varpro.R` (add helper).
- Test: `tests/testthat/test_gg_partial_varpro.R`

- [ ] **Step 1: Write the failing test**

```r
## ── v3.3.0 default tau = median follow-up ────────────────────────────────────
test_that(".default_surv_tau is the median observed survival time", {
  skip_if_not_installed("randomForestSRC")
  m   <- make_mock_cpath()                          # veteran survival rfsrc
  tau <- ggRandomForests:::.default_surv_tau(list(rf = m$rf))
  expect_equal(tau, stats::median(survival::veteran$time))
})

test_that(".default_surv_tau falls back to median(time.interest)", {
  fake <- list(rf = list(time.interest = c(2, 4, 6, 8)))  # no yvar
  expect_equal(ggRandomForests:::.default_surv_tau(fake), 5)  # median(2,4,6,8)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run the test command. Expected: FAIL — `.default_surv_tau` not found.

- [ ] **Step 3: Add the helper**

In `R/gg_partial_varpro.R`, add near the survival learner:

```r
## Data-driven, units-safe default horizon for survival scales: the median
## observed follow-up time (the survival response's time column). Always in the
## model's own units, so it cannot mismatch them. Falls back to the median of
## the distinct event times if the raw response is not reachable.
#' @keywords internal
.default_surv_tau <- function(object) {
  rf  <- object$rf
  yv  <- rf$yvar
  tms <- NULL
  if (!is.null(yv)) {
    yv  <- as.data.frame(yv)
    # survival response time column is the first non-status numeric column;
    # randomForestSRC names it via yvar.names (time first, status second).
    tcol <- if (!is.null(rf$yvar.names)) rf$yvar.names[1] else names(yv)[1]
    tms  <- suppressWarnings(as.numeric(yv[[tcol]]))
  }
  if (is.null(tms) || !any(is.finite(tms))) tms <- rf$time.interest
  stats::median(tms, na.rm = TRUE)
}
```

- [ ] **Step 4: Run test to verify it passes**

Run the test command. Expected: both `.default_surv_tau` tests PASS.

- [ ] **Step 5: Commit**

```bash
git add R/gg_partial_varpro.R tests/testthat/test_gg_partial_varpro.R
git commit -m "feat(gg_partial_varpro): .default_surv_tau (median follow-up, units-safe)"
```

---

### Task 6: Routing + validation (survival surv learner, default τ, no errors)

Wire `surv` to the new learner on path A, default τ for `surv`/`rmst`, resolve `auto`+survival to `surv`, and drop the "requires time" stops.

**Files:**
- Modify: `R/gg_partial_varpro.R` — `gg_partial_varpro` (A-path routing), `.validate_varpro_inputs`, `.validate_rmst_inputs`, `.warn_varpro_rmst`.
- Test: `tests/testthat/test_gg_partial_varpro.R`

- [ ] **Step 1: Write the failing test**

```r
## ── v3.3.0 survival routing + default tau ────────────────────────────────────
test_that("gg_partial_varpro: scale='surv' computes via the learner, in [0,1]", {
  skip_on_cran()                            # varpro fit + partialpro (~15s)
  skip_if_not_installed("randomForestSRC"); skip_if_not_installed("varPro")
  set.seed(13)
  pbc <- get(utils::data("pbc", package = "randomForestSRC",
                         envir = environment()))
  pbc <- pbc[stats::complete.cases(pbc), ]
  vp  <- varPro::varpro(Surv(days, status) ~ ., pbc, ntree = 60, nvar = 2)

  # explicit tau
  r <- gg_partial_varpro(object = vp, scale = "surv", time = 1000, nvars = 1)
  expect_equal(attr(r, "provenance")$scale, "surv")
  expect_equal(attr(r, "provenance")$path, "A")           # learner path, not C
  expect_true(all(r$continuous$parametric >= 0 &
                  r$continuous$parametric <= 1))
  expect_true(all(is.na(r$continuous$causal)))            # bounded -> causal NA

  # default tau (no time): provenance records the median-follow-up tau
  rd <- suppressMessages(
    gg_partial_varpro(object = vp, scale = "surv", nvars = 1))
  expect_equal(attr(rd, "provenance")$rmst_tau,
               ggRandomForests:::.default_surv_tau(vp))

  # auto resolves to surv (not mortality, no error)
  ra <- suppressMessages(gg_partial_varpro(object = vp, nvars = 1))
  expect_equal(attr(ra, "provenance")$scale, "surv")
})

test_that("gg_partial_varpro: scale='surv' with precomputed part_dta still errors w/o object", {
  expect_error(
    gg_partial_varpro(part_dta = make_mock_vpro_data(), scale = "surv"),
    regexp = "requires 'object'"
  )
})
```

- [ ] **Step 2: Run test to verify it fails**

Run the test command. Expected: FAIL — `surv` currently routes through path C (provenance path `"C"`), no learner, no default τ.

- [ ] **Step 3: Re-route `surv`, add default τ, relax validation**

`scale` is already the resolved concrete scale here (Task 3 added the early
`.resolve_varpro_scale` overwrite), so every `scale == ...` / `scale %in% ...`
check below operates on `surv`/`rmst`/`chf`/`mortality`/`prob`/... never
`"auto"`.

In `R/gg_partial_varpro.R`:

(a) The C-path block currently catches `surv` and `chf`. Narrow it to `chf` only. Replace:

```r
  ## ---- C-path: route through gg_partial_rfsrc ----------------------------
  if (!is.null(object) && scale %in% c("surv", "chf")) {
    return(.gg_partial_varpro_cpath(object, scale, time, model))
  }
```

with:

```r
  ## ---- C-path: route CHF through gg_partial_rfsrc ------------------------
  ## (surv now uses the partialpro S(t) learner on path A, below.)
  if (!is.null(object) && scale == "chf") {
    return(.gg_partial_varpro_cpath(object, scale, time, model))
  }

  ## ---- Survival default horizon: surv/rmst fill tau from the data --------
  if (!is.null(object) && scale %in% c("surv", "rmst") && is.null(time)) {
    time <- .default_surv_tau(object)
    message("gg_partial_varpro: using default horizon tau = ", signif(time, 4),
            " (median follow-up). Set 'time' to choose another.")
  }
```

(b) The A-path recompute currently only branches `rmst`. Replace:

```r
  if (is.null(part_dta)) {
    part_dta <- if (scale == "rmst") {
      varPro::partialpro(object, learner = .rmst_learner(object, time), ...)
    } else {
      varPro::partialpro(object, ...)
    }
  }
```

with:

```r
  if (is.null(part_dta)) {
    learner <- switch(scale,
      rmst = .rmst_learner(object, time),
      surv = .surv_learner(object, time),
      NULL)
    part_dta <- if (is.null(learner)) {
      varPro::partialpro(object, ...)
    } else {
      varPro::partialpro(object, learner = learner, ...)
    }
  }
```

(c) In `.validate_varpro_inputs`, the `surv`/`chf` "requires object" check stays (we still need `object` for the learner). Change the message set unchanged but keep `surv` requiring `object`:

The existing check is already correct:

```r
  if (scale %in% c("surv", "chf") && is.null(object)) {
    stop("scale = '", scale, "' requires 'object' (the varpro fit)",
         call. = FALSE)
  }
```

Leave it as-is. (This is what the second new test asserts.)

(d) In `.validate_rmst_inputs`, **remove** the "requires time" stop (τ now defaults). Replace:

```r
#' @keywords internal
.validate_rmst_inputs <- function(part_dta, object, time) {
  if (is.null(time)) {
    stop("scale = 'rmst' requires 'time' (the RMST horizon tau)",
         call. = FALSE)
  }
  if (is.null(part_dta) && !is.null(object) &&
      !identical(object$family, "surv")) {
    stop("scale = 'rmst' requires a survival varpro fit ",
         "(object$family == \"surv\")", call. = FALSE)
  }
  invisible(NULL)
}
```

with:

```r
#' @keywords internal
.validate_rmst_inputs <- function(part_dta, object, time) {
  # tau is optional now (defaults to median follow-up when recomputing from
  # an object); only the survival-family requirement remains.
  if (is.null(part_dta) && !is.null(object) &&
      !identical(object$family, "surv")) {
    stop("scale = 'rmst' requires a survival varpro fit ",
         "(object$family == \"surv\")", call. = FALSE)
  }
  invisible(NULL)
}
```

(e) `.validate_varpro_inputs` currently calls `.validate_rmst_inputs` only for `scale == "rmst"`. Extend it to also guard `surv` recompute needs a survival fit. Replace:

```r
  if (scale == "rmst") .validate_rmst_inputs(part_dta, object, time)
```

with:

```r
  if (scale %in% c("rmst", "surv")) .validate_rmst_inputs(part_dta, object, time)
```

(f) `.warn_varpro_rmst` references `scale == "rmst"` for the precomputed-part_dta and out-of-range warnings. Generalise its guard to both survival learner scales. Change the opening:

```r
.warn_varpro_rmst <- function(part_dta, object, scale, time) {
  if (scale == "rmst") {
```

to:

```r
.warn_varpro_rmst <- function(part_dta, object, scale, time) {
  if (scale %in% c("rmst", "surv")) {
```

(The "tau exceeds largest event time" branch is harmless for `surv` — S(τ) just snaps to the last column; the warning still informs.)

- [ ] **Step 4: Run test to verify it passes**

Run the test command. Expected: the routing tests PASS (the e2e one runs because `NOT_CRAN=true`). Re-run the *whole* file; the previously-added `scale='rmst'` tests that asserted "requires 'time'" must be reconciled — see Step 4b.

- [ ] **Step 4b: Fix the now-obsolete rmst "requires time" test**

The file has an older test:

```r
test_that("gg_partial_varpro: scale='rmst' without time → stop", {
  expect_error(
    gg_partial_varpro(part_dta = make_mock_vpro_data(), scale = "rmst"),
    regexp = "requires 'time'"
  )
})
```

RMST no longer errors on missing `time` when recomputing — but this call passes only `part_dta` (no `object`), so nothing recomputes and τ is irrelevant; it should now succeed as a label-only path. Replace that test with:

```r
test_that("gg_partial_varpro: scale='rmst' part_dta-only no longer needs time", {
  # tau defaults when recomputing from object; with part_dta only there is
  # nothing to recompute, so this is a label-only call and must not error.
  expect_no_error(suppressWarnings(
    gg_partial_varpro(part_dta = make_mock_vpro_data(), scale = "rmst")))
})
```

Re-run the file; expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/gg_partial_varpro.R tests/testthat/test_gg_partial_varpro.R
git commit -m "feat(gg_partial_varpro): surv via learner on path A; surv/rmst default tau; relax validation"
```

---

### Task 7: Target class, provenance, and y-axis labels

Resolve the classification target class, record it in provenance, and build the new y-labels.

**Files:**
- Modify: `R/gg_partial_varpro.R` — add `.varpro_target`, extend `.varpro_provenance`.
- Modify: `R/plot.gg_partial_varpro.R` — `.partial_varpro_ylabel`.
- Test: `tests/testthat/test_gg_partial_varpro.R`

- [ ] **Step 1: Write the failing test**

```r
## ── v3.3.0 labels + provenance target ────────────────────────────────────────
test_that(".partial_varpro_ylabel: prob/odds/logodds with target class", {
  lab <- ggRandomForests:::.partial_varpro_ylabel
  expect_match(lab(list(scale = "prob", target = "yes")),     "P\\(Y = yes\\)")
  expect_match(lab(list(scale = "odds", target = "yes")),     "Odds\\(Y = yes\\)")
  expect_match(lab(list(scale = "logodds", target = "yes")),  "Log-odds")
  expect_equal(lab(list(scale = "prob", target = NA)),        "Probability")
  expect_match(lab(list(scale = "surv", rmst_tau = 1000)),
               "Survival probability at t = 1000")
})

test_that("gg_partial_varpro: classification provenance records target class", {
  skip_on_cran(); skip_if_not_installed("varPro")
  set.seed(5)
  dat <- data.frame(y = factor(rep(c("a", "b"), 60)),
                    x1 = rnorm(120), x2 = rnorm(120))
  vp  <- varPro::varpro(y ~ ., dat, ntree = 40, nvar = 2)
  r   <- gg_partial_varpro(object = vp, scale = "prob", nvars = 1)
  expect_equal(attr(r, "provenance")$scale, "prob")
  expect_equal(attr(r, "provenance")$target, "b")   # last factor level default
})
```

- [ ] **Step 2: Run test to verify it fails**

Run the test command. Expected: FAIL — `.partial_varpro_ylabel` has no `prob/odds/logodds/surv` arms; provenance has no `target`.

- [ ] **Step 3a: Add `.varpro_target` and extend provenance**

In `R/gg_partial_varpro.R`, add:

```r
## Classification target class label: the `target` passed through ... if any,
## else the last factor level of the response (partialpro's default target).
## NA for non-classification fits or when only part_dta is supplied.
#' @keywords internal
.varpro_target <- function(object, dots) {
  if (is.null(object) || !identical(object$family, "class")) return(NA_character_)
  if (!is.null(dots$target)) return(as.character(dots$target))
  lv <- levels(object$y.org)
  if (is.null(lv)) lv <- levels(as.factor(object$y))
  if (is.null(lv)) NA_character_ else lv[length(lv)]
}
```

In `.varpro_provenance`, add a `target` slot. Change the signature and body. Replace:

```r
#' @keywords internal
.varpro_provenance <- function(object, scale, time, path = "A") {
  prov_family <- if (!is.null(object)) object$family     else NA_character_
  prov_xvars  <- if (!is.null(object)) object$xvar.names else NA_character_
  prov_n      <- if (!is.null(object)) nrow(object$x)    else NA_integer_
  prov_ntree  <- if (!is.null(object)) object$max.tree   else NA_integer_
  scale_used  <- .resolve_varpro_scale(scale, prov_family)
  list(
    source     = "varPro",
    family     = prov_family,
    ntree      = prov_ntree,
    n          = prov_n,
    scale      = scale_used,
    rmst_tau   = time,
    xvar.names = prov_xvars,
    path       = path
  )
}
```

with:

```r
#' @keywords internal
.varpro_provenance <- function(object, scale, time, path = "A", target = NA) {
  prov_family <- if (!is.null(object)) object$family     else NA_character_
  prov_xvars  <- if (!is.null(object)) object$xvar.names else NA_character_
  prov_n      <- if (!is.null(object)) nrow(object$x)    else NA_integer_
  prov_ntree  <- if (!is.null(object)) object$max.tree   else NA_integer_
  scale_used  <- .resolve_varpro_scale(scale, prov_family)
  list(
    source     = "varPro",
    family     = prov_family,
    ntree      = prov_ntree,
    n          = prov_n,
    scale      = scale_used,
    rmst_tau   = time,
    target     = target,
    xvar.names = prov_xvars,
    path       = path
  )
}
```

In `gg_partial_varpro`, the A-path builds provenance. Find:

```r
  prov <- .varpro_provenance(object, scale, time, path = "A")
```

and replace with (capture target from `...`, after the `time` default is set in Task 6 so `rmst_tau` records the resolved τ):

```r
  prov <- .varpro_provenance(object, scale, time, path = "A",
                             target = .varpro_target(object, list(...)))
```

- [ ] **Step 3b: New y-axis labels**

In `R/plot.gg_partial_varpro.R`, replace the `.partial_varpro_ylabel` `switch` body to add the classification + survival arms. Replace:

```r
  switch(scale,
    mortality = "Ensemble mortality (expected events)",
    rmst      = {
      tau <- prov$rmst_tau
      if (!is.null(tau) && !is.na(tau)) {
        sprintf("RMST (τ = %g)", tau)
      } else {
        "RMST"
      }
    },
    surv      = {
      t <- prov$rmst_tau
      if (!is.null(t) && !is.na(t)) {
        sprintf("Survival probability at t = %g", t)
      } else {
        "Survival probability"
      }
    },
    chf       = {
      t <- prov$rmst_tau
      if (!is.null(t) && !is.na(t)) {
        sprintf("Cumulative hazard at t = %g", t)
      } else {
        "Cumulative hazard"
      }
    },
    "Partial Effect"   # generic / auto-regr / auto-class / unknown
  )
```

with:

```r
  tgt <- prov$target
  has_tgt <- !is.null(tgt) && !is.na(tgt)
  switch(scale,
    prob      = if (has_tgt) sprintf("P(Y = %s)", tgt) else "Probability",
    odds      = if (has_tgt) sprintf("Odds(Y = %s)", tgt) else "Odds",
    logodds   = if (has_tgt) sprintf("Log-odds(Y = %s)", tgt) else "Log-odds",
    mortality = "Ensemble mortality (expected events)",
    rmst      = {
      tau <- prov$rmst_tau
      if (!is.null(tau) && !is.na(tau)) sprintf("RMST (τ = %g)", tau)
      else "RMST"
    },
    surv      = {
      t <- prov$rmst_tau
      if (!is.null(t) && !is.na(t)) sprintf("Survival probability at t = %g", t)
      else "Survival probability"
    },
    chf       = {
      t <- prov$rmst_tau
      if (!is.null(t) && !is.na(t)) sprintf("Cumulative hazard at t = %g", t)
      else "Cumulative hazard"
    },
    "Partial Effect"   # generic / regr / unknown
  )
```

- [ ] **Step 4: Run test to verify it passes**

Run the test command. Expected: the label + provenance-target tests PASS.

- [ ] **Step 5: Commit**

```bash
git add R/gg_partial_varpro.R R/plot.gg_partial_varpro.R tests/testthat/test_gg_partial_varpro.R
git commit -m "feat(gg_partial_varpro): target-class provenance + prob/odds/surv y-labels"
```

---

### Task 8: Plot drops + warns on causal for bounded scales

When the scale is bounded, `causal` is all-`NA`; the plot must not draw it and must warn if the user explicitly asked for `type = "causal"`.

**Files:**
- Modify: `R/plot.gg_partial_varpro.R` — `plot.gg_partial_varpro` (A-path, after `match.arg(type)`).
- Test: `tests/testthat/test_gg_partial_varpro.R`

- [ ] **Step 1: Write the failing test**

```r
## ── v3.3.0 plot: causal hidden on bounded scales ─────────────────────────────
test_that("plot.gg_partial_varpro: bounded scale drops causal, warns if asked", {
  d <- make_mock_vpro_data()
  res <- gg_partial_varpro(d, nvars = 1, scale = "prob")
  # default type: no error, causal silently absent
  gg <- plot(res)
  expect_s3_class(gg, "ggplot")
  # explicit causal -> warning + still returns a ggplot (par/nonpar only)
  expect_warning(plot(res, type = "causal"), regexp = "causal")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run the test command. Expected: FAIL — `plot` currently passes `type` straight to `pivot_longer`; `type = "causal"` on an all-NA column produces an all-NA line, no warning (and `type` default still includes causal, which would draw an all-NA layer).

- [ ] **Step 3: Drop/warn causal on bounded scales**

In `R/plot.gg_partial_varpro.R`, in `plot.gg_partial_varpro`, just after:

```r
  ## A-path rendering.
  type   <- match.arg(type, several.ok = TRUE)
  ylabel <- .partial_varpro_ylabel(prov)
```

insert:

```r
  ## On bounded scales (prob/odds/surv) the causal contrast is not shown
  ## (it cannot share the level axis). Drop it; warn only if asked explicitly.
  if (!is.null(prov) && .is_bounded_scale(prov$scale %||% "generic")) {
    if ("causal" %in% type) {
      warning("plot.gg_partial_varpro: 'causal' is not shown on the ",
              prov$scale, " scale (it is a contrast, not a level). ",
              "Use scale = 'logodds' (classification) or 'mortality'/'rmst' ",
              "(survival) to see it.", call. = FALSE)
    }
    type <- setdiff(type, "causal")
  }
```

(`.is_bounded_scale` is defined in `R/gg_partial_varpro.R`; both files are in the same package namespace, so no import is needed.)

- [ ] **Step 4: Run test to verify it passes**

Run the test command. Expected: the plot causal test PASSES.

- [ ] **Step 5: Commit**

```bash
git add R/plot.gg_partial_varpro.R tests/testthat/test_gg_partial_varpro.R
git commit -m "feat(plot.gg_partial_varpro): drop/warn causal on bounded scales"
```

---

### Task 9: Documentation, references, NEWS

Roxygen `@section`s, `@details`, `@param scale` update, the virtual-twins reference, and NEWS bullets.

**Files:**
- Modify: `R/gg_partial_varpro.R` (roxygen header), `R/plot.gg_partial_varpro.R` (roxygen header), `NEWS.md`.
- Regenerate: `man/` via `devtools::document()`.

- [ ] **Step 1: Update `@param scale` and `@details` on `gg_partial_varpro`**

In the `R/gg_partial_varpro.R` roxygen header, replace the `@param scale` block:

```r
#' @param scale Character; sets the y-axis label and, for survival forests,
#'   the output type.  One of \code{"auto"} (default), \code{"mortality"},
#'   \code{"rmst"}, \code{"surv"}, or \code{"chf"}.
```

with:

```r
#' @param scale Character; the y-axis scale.  One of \code{"auto"} (default),
#'   the classification scales \code{"prob"} / \code{"odds"} / \code{"logodds"},
#'   or the survival scales \code{"rmst"} / \code{"surv"} / \code{"mortality"} /
#'   \code{"chf"}.  With \code{"auto"}: classification fits resolve to
#'   \code{"prob"} (probability of the target class) and survival fits to
#'   \code{"surv"} (survival probability at a default horizon \eqn{\tau}); see
#'   \strong{Details}.
```

Add a `@details` block (after the existing RMST details paragraph):

```r
#' **Classification scale (scale = "prob"/"odds"/"logodds"):**
#' \code{varPro::partialpro} returns classification effects as \emph{log-odds}
#' of the target class.  \code{scale = "prob"} (the classification default)
#' back-transforms to probability \eqn{P(Y = \mathrm{target})}, \code{"odds"} to
#' the odds, and \code{"logodds"} keeps the raw scale.  The back-transform is
#' applied per observation \emph{before} averaging, so the curve is the mean
#' predicted probability, not the probability of the mean log-odds.  The
#' \code{causal} contrast is shown only on \code{"logodds"} (see
#' \code{\link{plot.gg_partial_varpro}}).
#'
#' **Survival scale (scale = "surv"):** \code{scale = "surv"} computes survival
#' probability \eqn{S(\tau \mid x)} through \code{partialpro} (the same UVT
#' engine as mortality and RMST), bounded in [0, 1].  When \code{time} is not
#' supplied, \eqn{\tau} defaults to the \strong{median follow-up time} of the
#' fit -- a data-driven horizon that is always in the model's own time units,
#' so it cannot be mis-specified the way a hand-typed \eqn{\tau} can.  The
#' resolved \eqn{\tau} is reported in a message and the axis label; pass
#' \code{time = tau} to choose another.  \code{scale = "mortality"} keeps the
#' unbounded ensemble-mortality score as an explicit opt-in.
```

- [ ] **Step 2: Add the `@section`s and reference on `plot.gg_partial_varpro`**

In `R/plot.gg_partial_varpro.R` roxygen header, after the existing
"What this tells you" section, add (the prose was approved in the spec):

```r
#' @section Reading a probability curve (scale = "prob"):
#' The y-axis is \eqn{P(Y = \mathrm{target})}, the model's predicted probability
#' of the target class as the focal variable varies (others held at their
#' UVT-plausible average).  \code{"odds"} and \code{"logodds"} are the same
#' curve on the odds and log-odds scales.  The \code{causal} curve is a
#' contrast (below) and is \emph{not} shown on \code{"prob"}/\code{"odds"};
#' use \code{"logodds"} to see it.
#'
#' @section Reading a survival-probability curve (scale = "surv"):
#' The y-axis is \eqn{S(\tau \mid x)}, the predicted probability of surviving
#' past \eqn{\tau}, bounded in [0, 1] and read in the model's time units.
#' Higher is better (more survival).  \eqn{\tau} defaults to the median
#' follow-up time when not supplied.
#'
#' @section What the causal curve is, and when to use it:
#' \code{causal} is the \strong{baseline-subtracted local effect} -- varPro's
#' virtual- ("digital-") twins estimator (Ishwaran & Blackstone, 2025).  It
#' shows how the prediction shifts as the focal variable moves away from the
#' reference grid point, with the other covariates held at on-manifold
#' (UVT-plausible) values; it is a \strong{contrast} (it starts at 0), not a
#' level.  Use it when you want the local effect (change-from-baseline) rather
#' than the absolute predicted level, and as a cross-check on the parametric
#' and nonparametric curves.  It is varpro's local estimator \emph{within the
#' fitted model}, \strong{not a structural causal claim} about the
#' data-generating process.  Because it is a contrast it cannot share a
#' probability/odds axis with the absolute curves, so it is shown only on the
#' additive scales (\code{"logodds"}, \code{"mortality"}, \code{"rmst"}).
```

Add to the `@references` of BOTH `R/gg_partial_varpro.R` and
`R/plot.gg_partial_varpro.R` (alongside the existing Ishwaran et al. 2008 ref):

```r
#' Ishwaran H, Blackstone EH (2025).
#' Harnessing the power of virtual (digital) twins: Graphical causal tools for
#' understanding patient and hospital differences.
#' \emph{Computational and Structural Biotechnology Journal}, \bold{28}, 312.
```

- [ ] **Step 3: NEWS bullets**

In `NEWS.md`, under the `ggRandomForests v3.3.0` heading, add above the existing
RMST-docs bullet:

```
* `gg_partial_varpro()`: **classification partial plots now default to
  probability.** `scale = "auto"` on a classification fit resolves to `"prob"`
  (P(Y = target class)) instead of raw log-odds; `"odds"` and `"logodds"` are
  options. The back-transform is applied before averaging (mean predicted
  probability). The `causal` contrast is shown only on `"logodds"`.
* `gg_partial_varpro()`: **survival partial plots now default to survival
  probability.** `scale = "auto"` on a survival fit resolves to `"surv"`
  (S(tau | x), bounded 0-1) via a new partialpro learner, instead of the
  unbounded ensemble-mortality score (still available via
  `scale = "mortality"`). `"surv"` and `"rmst"` default `tau` to the median
  follow-up time when `time` is omitted -- a units-safe, data-driven horizon
  (v3.2.0's `rmst` required `time`; this is a loosening). The resolved `tau` is
  reported in a message and the axis label.
* `plot.gg_partial_varpro()`: documents what the `causal` (virtual-twins)
  estimator is and when to use it, and explains why it is hidden on the bounded
  probability scales.
```

- [ ] **Step 4: Regenerate docs and verify clean**

Run:

```bash
Rscript -e 'devtools::document()'
```

Expected: writes `man/gg_partial_varpro.Rd` and `man/plot.gg_partial_varpro.Rd`, no warnings.

- [ ] **Step 5: Commit**

```bash
git add R/gg_partial_varpro.R R/plot.gg_partial_varpro.R NEWS.md man/
git commit -m "docs(gg_partial_varpro): scale @details, prob/surv/causal @sections, NEWS, virtual-twins ref"
```

---

### Task 10: Full verification (self-review, suite, lint, R CMD check)

**Files:** none new — verification only.

- [ ] **Step 1: Run the full test file**

```bash
NOT_CRAN=true Rscript -e 'suppressMessages(devtools::load_all(quiet=TRUE)); testthat::test_file("tests/testthat/test_gg_partial_varpro.R", reporter="summary")'
```

Expected: all PASS / SKIP, 0 FAIL. Investigate any failure before proceeding.

- [ ] **Step 2: Reconcile sibling test files**

```bash
NOT_CRAN=true Rscript -e 'suppressMessages(devtools::load_all(quiet=TRUE)); testthat::test_file("tests/testthat/test_gg_partialpro.R", reporter="minimal"); testthat::test_file("tests/testthat/test_ggrandomforests_news.R", reporter="minimal")'
```

Expected: PASS. The news test greps `NEWS.md` for the DESCRIPTION version (3.3.0) — already aligned.

- [ ] **Step 3: Lint the changed files**

```bash
Rscript -e 'cat(length(lintr::lint("R/gg_partial_varpro.R")), length(lintr::lint("R/plot.gg_partial_varpro.R")), "issues\n")'
```

Expected: `0 0 issues`. If `cyclocomp` fires on `gg_partial_varpro` (it grew), extract the new routing block into a small `.resolve_partial_part_dta(object, scale, time, ...)` helper and re-run.

- [ ] **Step 4: Spec coverage self-check**

Confirm each spec section maps to a task: 3a/3b → T1; 3c → T2,T3; 3d → T3 (NA) + T8 (warn) + T9 (docs); 3e → T4,T5,T6; 3f → T7; 3g → T7; §4 docs → T9; §5 tests → distributed. No gaps.

- [ ] **Step 5: Full CRAN check (no submission)**

```bash
R CMD build . && R CMD check --as-cran ggRandomForests_3.3.0.tar.gz
```

Expected: `Status: OK` (0/0/0), manual builds. (The tarball is gitignored.) Note check time stays < 10 min.

- [ ] **Step 6: Commit any verification fixes**

```bash
git add -A
git commit -m "test/chore: 3.3.0 scale work — full suite + check green"
```

---

## Notes for the implementer

- The repo skips varpro forest grows on CRAN (gcc-UBSAN upstream issue); keep the heavy end-to-end fits behind `skip_on_cran()`, and exercise the pure helpers (`.scale_transform`, `.surv_at_tau`, `.default_surv_tau`, `.resolve_varpro_scale`, `.partial_varpro_ylabel`) with mock inputs that run on CRAN.
- `%||%` is defined in the package (`R/print_helpers.R`); usable in both files.
- Do NOT `git add -A` blind — the working tree has gitignored build artifacts (`.superpowers/`, vignette HTML, `*.tar.gz`); stage explicit paths.
- Two deliberate behaviour changes ship here (classification default → prob; survival default → surv, no more mortality default). They are documented in NEWS and are the headline of 3.3.0.
