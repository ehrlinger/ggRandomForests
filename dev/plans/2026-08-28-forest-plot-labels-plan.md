# Forest Plot Labels and Importance Ordering — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `plot()` on forest objects produce a deliverable-quality figure by default — human-readable variable labels, facets in varPro importance order, and a loud warning when the y-axis scale could not be determined.

**Architecture:** Two new internal helpers in `R/utils.R` (`.forest_labels()` for label resolution, `.varpro_importance_order()` for ranking). Ordering is applied in the **constructor** (`gg_partial_varpro()` sets `name` as an ordered factor); labels are applied in the **plot methods** (a new `labels =` argument, resolved at draw time). Returned objects keep raw column names so downstream consumers are unaffected.

**Tech Stack:** R (>= 4.4.0), ggplot2, dplyr, tidyr, patchwork, varPro, testthat (in `tests/testthat/`), roxygen2, lintr.

## Global Constraints

- **Repo/branch:** worktree `~/Documents/GitHub/ggRandomForests-labels`, branch `feat/forest-plot-labels`. Never push to `main`; open a PR and let the maintainer merge.
- **No version bump.** `DESCRIPTION` stays at `4.0.0`. Add NEWS bullets to the **end** of the `ggRandomForests v4.0.0 (development)` section in `NEWS.md`.
- **No new package dependencies.** `.forest_labels()` is base R only. Do not add `labelled`, `yaml`, `ggh4x`, or `hvtiRutilities`.
- **Roxygen:** `Roxygen: list(markdown = TRUE)` is set, but the codebase writes Rd macros (`\code{}`, `\emph{}`, `\link{}`). **Match the surrounding file's style** — use Rd macros, not markdown.
- **Lint:** `.lintr` sets `line_length_linter(120)` and `cyclocomp_linter(complexity_limit = 20)`. `lintr::lint_package()` must return zero before pushing.
- **Test files:** `tests/testthat/test_*.R` — underscore, not hyphen.
- **Test quality:** assert on **data**, not `expect_s3_class(p, "ggplot")`. Use `ggplot2::ggplot_build()` or inspect the built data frames.
- **Out of scope, do not touch:** `R/gg_rhf_importance.R`, `R/plot.gg_rhf_importance.R` (a parallel session owns these); the three-series overlay annotation; the categorical/continuous split.
- **Run after any roxygen change:** `devtools::document()`, and commit `man/` and `NAMESPACE` with the source change.

---

## File Structure

| File | Responsibility | Action |
|---|---|---|
| `R/utils.R` | `.forest_labels()`, `.forest_labels_check()`, `.apply_forest_labels()`, `.varpro_importance_order()`, `.varpro_rank_of()` | Modify (append) |
| `R/gg_partial_varpro.R` | Set `name` as an ordered factor; slice `nvars` after ranking | Modify |
| `R/plot.gg_partial_varpro.R` | `labels =` argument; loud generic-scale warning | Modify |
| `R/plot.gg_partial.R` | `labels =` on `plot.gg_partial()` and the `plot.gg_partialpro()` shim | Modify |
| `R/plot.gg_vimp.R` | `labels =`; deprecate `lbls`; remove the length gate | Modify |
| `R/plot.gg_varpro.R` | `labels =` on the variable axis | Modify |
| `R/autoplot_methods.R` | Forward `labels =` through the autoplot shims | Modify |
| `tests/testthat/test_forest_labels.R` | `.forest_labels()` unit tests | Create |
| `tests/testthat/test_varpro_importance_order.R` | ranking unit tests | Create |
| `tests/testthat/test_gg_partial_varpro.R` | ordering + nvars integration | Modify |
| `tests/testthat/test_gg_vimp.R` | labels, deprecation, length-gate regression | Modify or create |
| `NEWS.md` | Release notes | Modify |

---

## Task 1: `.forest_labels()` — label resolution

**Files:**
- Modify: `R/utils.R` (append at end)
- Test: `tests/testthat/test_forest_labels.R` (create)

**Interfaces:**
- Consumes: nothing.
- Produces:
  - `.forest_labels(labels)` → named `character` (names = variable, values = label), or `NULL` when `labels` is `NULL`.
  - `.apply_forest_labels(vars, lookup)` → `character` the same length as `vars`, unmatched entries falling back to the raw name.

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test_forest_labels.R`:

```r
test_that(".forest_labels returns NULL for NULL", {
  expect_null(.forest_labels(NULL))
})

test_that(".forest_labels accepts a named character vector", {
  out <- .forest_labels(c(bpd_last = "BP Diastole", vis_last = "VIS"))
  expect_equal(out[["bpd_last"]], "BP Diastole")
  expect_equal(out[["vis_last"]], "VIS")
})

test_that(".forest_labels reads attr(col, 'label') from a labelled data frame", {
  d <- data.frame(age = 1:3, bpd = 4:6)
  attr(d$age, "label") <- "Age at operation"
  attr(d$bpd, "label") <- "BP Diastole"
  out <- .forest_labels(d)
  expect_equal(out[["age"]], "Age at operation")
  expect_equal(out[["bpd"]], "BP Diastole")
})

test_that(".forest_labels accepts a key/label data frame", {
  m <- data.frame(key = c("age", "bpd"),
                  label = c("Age at operation", "BP Diastole"),
                  stringsAsFactors = FALSE)
  out <- .forest_labels(m)
  expect_equal(out[["bpd"]], "BP Diastole")
})

test_that(".forest_labels prefers the key/label shape over attribute reading", {
  m <- data.frame(key = "age", label = "From key/label", stringsAsFactors = FALSE)
  attr(m$key, "label") <- "From attribute"
  out <- .forest_labels(m)
  expect_equal(out[["age"]], "From key/label")
})

test_that(".forest_labels warns when nothing resolves", {
  d <- data.frame(age = 1:3, bpd = 4:6)   # no label attributes
  expect_warning(.forest_labels(d), "No variable labels")
})

test_that(".forest_labels rejects an unnamed character vector", {
  expect_error(.forest_labels(c("BP Diastole")), "must be a named character vector")
})

test_that(".apply_forest_labels falls back per variable", {
  lookup <- c(bpd_last = "BP Diastole")
  out <- .apply_forest_labels(c("bpd_last", "vis_last"), lookup)
  expect_equal(out, c("BP Diastole", "vis_last"))
})

test_that(".apply_forest_labels is identity when lookup is NULL", {
  expect_equal(.apply_forest_labels(c("a", "b"), NULL), c("a", "b"))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test_forest_labels.R")'`
Expected: FAIL — `could not find function ".forest_labels"`

- [ ] **Step 3: Write the implementation**

Append to `R/utils.R`:

```r
## ---------------------------------------------------------------------------
## Variable-label resolution, shared by every plot method that draws variable
## names.  Base R only: 'ggRandomForests' is on CRAN and must not take a
## dependency on 'labelled' or on an internal package for a cosmetic feature.
##
## Three input shapes are accepted, because attr(x, "label") is a haven/SAS-era
## carrier that does not reliably survive a parquet round-trip.  The named-vector
## and key/label arms are format-agnostic and are the durable ones.
#' @keywords internal
.forest_labels <- function(labels) {
  if (is.null(labels)) {
    return(NULL)
  }

  ## Shape 3 first: a two-column key/label lookup (the shape
  ## hvtiRutilities::label_map() returns).  Checked before attribute reading so
  ## a lookup table is never mistaken for a labelled data frame.
  if (is.data.frame(labels) && all(c("key", "label") %in% names(labels))) {
    out <- as.character(labels[["label"]])
    names(out) <- as.character(labels[["key"]])
    return(.forest_labels_check(out))
  }

  ## Shape 1: a labelled data frame -- read attr(col, "label") per column.
  if (is.data.frame(labels)) {
    out <- vapply(labels, function(col) {
      lb <- attr(col, "label")
      if (is.null(lb) || !nzchar(as.character(lb)[1L])) {
        NA_character_
      } else {
        as.character(lb)[1L]
      }
    }, character(1L))
    return(.forest_labels_check(out[!is.na(out)]))
  }

  ## Shape 2: a named character vector.
  if (is.character(labels) && !is.null(names(labels))) {
    return(.forest_labels_check(labels))
  }

  stop("'labels' must be a named character vector, a labelled data frame, ",
       "or a two-column key/label data frame.", call. = FALSE)
}

## Warn once when a lookup resolves nothing at all.  The usual cause is that
## 'label' attributes were dropped in transit (a parquet round-trip written by a
## non-R stage does this silently), which otherwise presents as a figure that is
## simply unlabelled with no explanation.
#' @keywords internal
.forest_labels_check <- function(x) {
  if (length(x) == 0L) {
    warning("No variable labels were found. If the data came through a ",
            "parquet round-trip, 'label' attributes may have been dropped; ",
            "pass a named character vector instead.", call. = FALSE)
  }
  x
}

## Map raw variable names onto display labels, falling back per variable.  A
## variable with no label keeps its raw name: never blank, never an error.
#' @keywords internal
.apply_forest_labels <- function(vars, lookup) {
  vars <- as.character(vars)
  if (is.null(lookup) || length(lookup) == 0L) {
    return(vars)
  }
  out <- unname(lookup[vars])
  out[is.na(out)] <- vars[is.na(out)]
  out
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test_forest_labels.R")'`
Expected: PASS, 9 tests, 0 failures

- [ ] **Step 5: Lint and commit**

```bash
cd ~/Documents/GitHub/ggRandomForests-labels
Rscript -e 'print(lintr::lint("R/utils.R"))'
git add R/utils.R tests/testthat/test_forest_labels.R
git commit -m "feat: add .forest_labels() label resolution helper"
```

---

## Task 2: `.varpro_importance_order()` — importance ranking

**Files:**
- Modify: `R/utils.R` (append at end)
- Test: `tests/testthat/test_varpro_importance_order.R` (create)

**Interfaces:**
- Consumes: nothing from Task 1.
- Produces:
  - `.varpro_importance_order(part_dta, object)` → `character`, a permutation of `names(part_dta)`.
  - `.varpro_rank_of(nms, ranked)` → `numeric` the same length as `nms`; `Inf` for unranked.

**Background the implementer needs:** `varPro::get.topvars(object)` returns variable names ranked by importance, and returns **far fewer** names than the fit reaches — on a 3-variable test fit it returned one. Unranked variables are therefore the common case and must be preserved, not dropped. `object$xvar.org.names` and `object$xvar.names` are **not** parallel vectors (a factor may appear in one and not the other), so neither can index the other.

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test_varpro_importance_order.R`:

```r
test_that(".varpro_rank_of matches exactly", {
  expect_equal(unname(.varpro_rank_of(c("bpd", "age"), c("age", "bpd"))), c(2, 1))
})

test_that(".varpro_rank_of returns Inf for unranked names", {
  expect_equal(unname(.varpro_rank_of("vis", c("age", "bpd"))), Inf)
})

test_that(".varpro_rank_of resolves one-hot names by digit suffix", {
  # get.topvars() gave sex0/sex1; part_dta carries the original 'sex'
  expect_equal(unname(.varpro_rank_of("sex", c("age", "sex0", "sex1"))), 2)
})

test_that(".varpro_rank_of prefers an exact match over a prefix match", {
  # 'age' must not be captured by 'age_group'; and a digit suffix is required,
  # so 'age_group' can never be a one-hot level of 'age'.
  expect_equal(unname(.varpro_rank_of("age", c("age_group", "age"))), 2)
})

test_that(".varpro_importance_order returns list order when object is NULL", {
  pd <- list(age = 1, bpd = 2, vis = 3)
  expect_equal(.varpro_importance_order(pd, NULL), c("age", "bpd", "vis"))
})

test_that(".varpro_importance_order ranks by get.topvars and appends the rest", {
  pd <- list(age = 1, bpd = 2, vis = 3)
  fake <- structure(list(), class = "varpro")
  local_mocked_bindings(get.topvars = function(...) c("vis", "bpd"),
                        .package = "varPro")
  expect_equal(.varpro_importance_order(pd, fake), c("vis", "bpd", "age"))
})

test_that(".varpro_importance_order drops nothing", {
  pd <- list(a = 1, b = 2, c = 3, d = 4)
  fake <- structure(list(), class = "varpro")
  local_mocked_bindings(get.topvars = function(...) c("c"), .package = "varPro")
  expect_setequal(.varpro_importance_order(pd, fake), names(pd))
  expect_equal(length(.varpro_importance_order(pd, fake)), 4L)
})

test_that(".varpro_importance_order keeps list order among unranked names", {
  pd <- list(z = 1, y = 2, x = 3)
  fake <- structure(list(), class = "varpro")
  local_mocked_bindings(get.topvars = function(...) c("y"), .package = "varPro")
  expect_equal(.varpro_importance_order(pd, fake), c("y", "z", "x"))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test_varpro_importance_order.R")'`
Expected: FAIL — `could not find function ".varpro_rank_of"`

- [ ] **Step 3: Write the implementation**

Append to `R/utils.R`:

```r
## ---------------------------------------------------------------------------
## Rank the variables in a partialpro list by varPro importance.
##
## get.topvars() returns a SHORT ranked vector -- far shorter than the fit
## reaches -- so most names arrive unranked.  Those keep their incoming order and
## are appended after the ranked block; nothing is ever dropped.
#' @keywords internal
.varpro_importance_order <- function(part_dta, object) {
  nms <- names(part_dta)
  if (is.null(object) || is.null(nms)) {
    return(nms)
  }

  ranked <- tryCatch(as.character(varPro::get.topvars(object)),
                     error = function(e) character(0L))
  if (length(ranked) == 0L) {
    return(nms)
  }

  rank_key <- .varpro_rank_of(nms, ranked)
  ## seq_along() as the tiebreaker keeps incoming order stable among names that
  ## share a rank (in practice, all the unranked ones at Inf).
  nms[order(rank_key, seq_along(nms))]
}

## Position of each name in the ranked vector.  Exact match wins; failing that a
## one-hot level (name followed by digits, e.g. sex0/sex1) is accepted and the
## best -- lowest -- position across levels is taken.  Requiring digits keeps
## 'age' from being captured by 'age_group'.
#' @keywords internal
.varpro_rank_of <- function(nms, ranked) {
  vapply(nms, function(nm) {
    hit <- which(ranked == nm)
    if (length(hit) == 0L) {
      pat <- paste0("^", .escape_regex(nm), "[0-9]+$")
      hit <- grep(pat, ranked)
    }
    if (length(hit) == 0L) Inf else min(hit)
  }, numeric(1L))
}

#' @keywords internal
.escape_regex <- function(x) {
  gsub("([.\\\\|()\\[\\]{}^$*+?])", "\\\\\\1", x, perl = TRUE)
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test_varpro_importance_order.R")'`
Expected: PASS, 8 tests, 0 failures

If `local_mocked_bindings` is unavailable for `varPro::get.topvars`, replace those three tests with direct `.varpro_rank_of()` assertions plus one integration test in Task 3, and note the substitution in the commit message.

- [ ] **Step 5: Lint and commit**

```bash
cd ~/Documents/GitHub/ggRandomForests-labels
Rscript -e 'print(lintr::lint("R/utils.R"))'
git add R/utils.R tests/testthat/test_varpro_importance_order.R
git commit -m "feat: add .varpro_importance_order() ranking helper"
```

---

## Task 3: Wire ordering into the constructor (fixes the `nvars` bug)

**Files:**
- Modify: `R/gg_partial_varpro.R` — the `nvars` block in `gg_partial_varpro()`, plus `.build_varpro_dfs()` and `.process_cat_var()`
- Test: `tests/testthat/test_gg_partial_varpro.R` (append)

**Interfaces:**
- Consumes: `.varpro_importance_order(part_dta, object)` from Task 2.
- Produces: `x$continuous$name` and `x$categorical$name` are **factors** whose levels are in importance order. `facet_wrap(~name)` inherits that order with no change to the plot method.

**The bug being fixed:** the current code does `nvars <- length(part_dta)` then `.build_varpro_dfs()` loops `for (feature in seq(nvars))` — the **first** n list elements, before any ranking. `nvars = 10` therefore returns an arbitrary 10, not the top 10.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test_gg_partial_varpro.R`:

```r
make_mock_part_dta <- function(names_vec, n_obs = 20, n_pts = 12) {
  out <- lapply(names_vec, function(nm) {
    list(xvirtual    = seq_len(n_pts),
         xorg        = sample(seq_len(n_pts), n_obs, replace = TRUE),
         yhat.par    = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
         yhat.nonpar = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
         yhat.causal = matrix(rnorm(n_obs * n_pts), nrow = n_obs))
  })
  names(out) <- names_vec
  out
}

test_that("name is a factor so facet order is not alphabetical", {
  set.seed(42)
  pd <- make_mock_part_dta(c("vis", "age", "bpd"))
  res <- gg_partial_varpro(pd, cat_limit = 5)
  expect_s3_class(res$continuous$name, "factor")
  expect_equal(levels(res$continuous$name), c("vis", "age", "bpd"))
})

test_that("nvars selects the top n by importance, not the first n", {
  set.seed(42)
  pd <- make_mock_part_dta(c("age", "bpd", "vis"))
  fake <- structure(list(family = "class"), class = "varpro")
  local_mocked_bindings(get.topvars = function(...) c("vis", "bpd", "age"),
                        .package = "varPro")
  res <- gg_partial_varpro(pd, object = fake, scale = "logodds",
                           nvars = 2, cat_limit = 5)
  # Ranked order is vis, bpd, age -- so the top 2 are vis and bpd, NOT age/bpd.
  expect_setequal(levels(droplevels(res$continuous$name)), c("vis", "bpd"))
  expect_false("age" %in% as.character(res$continuous$name))
})

test_that("no object leaves part_dta list order intact", {
  set.seed(42)
  pd <- make_mock_part_dta(c("zulu", "alpha", "mike"))
  res <- gg_partial_varpro(pd, cat_limit = 5)
  expect_equal(levels(res$continuous$name), c("zulu", "alpha", "mike"))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test_gg_partial_varpro.R")'`
Expected: FAIL — `name` is character, and the `nvars` test picks `age`/`bpd`

- [ ] **Step 3: Reorder before slicing in `gg_partial_varpro()`**

In `R/gg_partial_varpro.R`, replace:

```r
  if (is.null(nvars)) {
    nvars <- length(part_dta)
  }
```

with:

```r
  ## Rank BEFORE slicing.  Slicing first would make nvars mean "the first n list
  ## elements", which is an arbitrary subset rather than the top n by importance.
  part_dta <- part_dta[.varpro_importance_order(part_dta, object)]

  if (is.null(nvars)) {
    nvars <- length(part_dta)
  }
  nvars <- min(nvars, length(part_dta))
```

- [ ] **Step 4: Make `name` an ordered factor in `.build_varpro_dfs()`**

In `R/gg_partial_varpro.R`, replace the tail of `.build_varpro_dfs()`:

```r
  list(
    continuous  = dplyr::bind_rows(cont_list),
    categorical = dplyr::bind_rows(cat_list)
  )
}
```

with:

```r
  ## 'name' must be a factor: as a character column facet_wrap() re-sorts it
  ## alphabetically and the importance order established above is discarded.
  lvls <- names(part_dta)[seq(nvars)]
  cont <- dplyr::bind_rows(cont_list)
  cats <- dplyr::bind_rows(cat_list)
  if (nrow(cont) > 0L) cont$name <- factor(cont$name, levels = lvls)
  if (nrow(cats) > 0L) cats$name <- factor(cats$name, levels = lvls)

  list(continuous = cont, categorical = cats)
}
```

`.process_cat_var()` needs no change — it writes a character `name` which the block above converts.

- [ ] **Step 5: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test_gg_partial_varpro.R")'`
Expected: PASS

- [ ] **Step 6: Run the full suite — this changes a returned object's column type**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'`
Expected: PASS. If any test compares `name` with `expect_equal(..., "age")`, update it to `as.character(...)`; a factor `name` is the intended new contract.

- [ ] **Step 7: Lint and commit**

```bash
cd ~/Documents/GitHub/ggRandomForests-labels
Rscript -e 'print(lintr::lint("R/gg_partial_varpro.R"))'
git add R/gg_partial_varpro.R tests/testthat/test_gg_partial_varpro.R
git commit -m "fix: rank varpro variables before slicing nvars, and order facets by importance"
```

---

## Task 4: Loud fallback when the scale cannot be resolved

**Files:**
- Modify: `R/gg_partial_varpro.R` — `.resolve_varpro_scale()`
- Test: `tests/testthat/test_gg_partial_varpro.R` (append)

**Interfaces:**
- Consumes: nothing.
- Produces: unchanged return value; adds a warning on the `"generic"` branch.

**Note:** this is **not** a bug fix. `"Partial Effect"` is an honest label for a scale that genuinely could not be determined. The change makes the silent path audible; the label and the defaults are untouched.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test_gg_partial_varpro.R`:

```r
test_that("auto scale warns when no fit is available to resolve it", {
  set.seed(42)
  pd <- make_mock_part_dta(c("age", "bpd"))
  expect_warning(gg_partial_varpro(pd, cat_limit = 5),
                 "scale could not be resolved")
})

test_that("an explicit scale does not warn", {
  set.seed(42)
  pd <- make_mock_part_dta(c("age", "bpd"))
  expect_no_warning(gg_partial_varpro(pd, scale = "logodds", cat_limit = 5))
})

test_that("a classification fit resolves auto to prob without warning", {
  fake <- structure(list(family = "class"), class = "varpro")
  expect_no_warning(sc <- .resolve_varpro_scale("auto", fake$family))
  expect_equal(sc, "prob")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test_gg_partial_varpro.R")'`
Expected: FAIL — no warning raised

- [ ] **Step 3: Write the implementation**

In `R/gg_partial_varpro.R`, replace `.resolve_varpro_scale()`:

```r
.resolve_varpro_scale <- function(scale, family) {
  if (scale != "auto") return(scale)
  if (is.na(family) || is.null(family)) return("generic")
  if (family == "surv")  return("surv")    # bounded survival default (3.3.0)
  if (family == "class") return("prob")    # probability default (3.3.0)
  "generic"   # regr or unknown
}
```

with:

```r
.resolve_varpro_scale <- function(scale, family) {
  if (scale != "auto") return(scale)
  ## No fit means no family, and no family means no scale.  Say so: the y-axis
  ## silently falls back to the generic "Partial Effect" label, which is honest
  ## but is rarely the scale a reader wants.
  if (is.na(family) || is.null(family)) {
    warning("gg_partial_varpro: scale could not be resolved because no ",
            "'object' was supplied; the y-axis will be labelled ",
            "'Partial Effect'. Pass object = <varpro fit> to get the ",
            "probability scale, or set 'scale' explicitly.", call. = FALSE)
    return("generic")
  }
  if (family == "surv")  return("surv")    # bounded survival default (3.3.0)
  if (family == "class") return("prob")    # probability default (3.3.0)
  "generic"   # regr or unknown
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'`
Expected: PASS. Existing tests that call `gg_partial_varpro(part_dta)` with no object and no scale will now emit this warning — wrap them in `suppressWarnings()` or pass `scale = "logodds"` explicitly. Do not silence it by weakening the warning.

- [ ] **Step 5: Lint and commit**

```bash
cd ~/Documents/GitHub/ggRandomForests-labels
Rscript -e 'print(lintr::lint("R/gg_partial_varpro.R"))'
git add R/gg_partial_varpro.R tests/testthat/test_gg_partial_varpro.R
git commit -m "feat: warn when the partial-dependence scale cannot be resolved"
```

---

## Task 5: `labels =` on `plot.gg_partial_varpro()`

**Files:**
- Modify: `R/plot.gg_partial_varpro.R`
- Modify: `R/plot.gg_partial.R` — the `plot.gg_partialpro()` shim at line ~266
- Modify: `R/autoplot_methods.R` — `autoplot.gg_partial_varpro()` and `autoplot.gg_partialpro()`
- Test: `tests/testthat/test_gg_partial_varpro.R` (append)

**Interfaces:**
- Consumes: `.forest_labels()`, `.apply_forest_labels()` from Task 1.
- Produces: `plot.gg_partial_varpro(x, type, labels = NULL, ...)`.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test_gg_partial_varpro.R`:

```r
test_that("labels rename facet strips without touching the data", {
  set.seed(42)
  pd  <- make_mock_part_dta(c("bpd", "vis"))
  res <- gg_partial_varpro(pd, scale = "logodds", cat_limit = 5)
  p   <- plot(res, labels = c(bpd = "BP Diastole", vis = "VIS"))

  strips <- ggplot2::ggplot_build(p)$layout$layout$name
  expect_true("BP Diastole" %in% as.character(strips))

  # The object itself must still carry raw names.
  expect_true("bpd" %in% as.character(res$continuous$name))
})

test_that("an unlabelled variable keeps its raw name", {
  set.seed(42)
  pd  <- make_mock_part_dta(c("bpd", "vis"))
  res <- gg_partial_varpro(pd, scale = "logodds", cat_limit = 5)
  p   <- plot(res, labels = c(bpd = "BP Diastole"))
  strips <- as.character(ggplot2::ggplot_build(p)$layout$layout$name)
  expect_true(all(c("BP Diastole", "vis") %in% strips))
})

test_that("labels = NULL reproduces the unlabelled plot", {
  set.seed(42)
  pd  <- make_mock_part_dta(c("bpd", "vis"))
  res <- gg_partial_varpro(pd, scale = "logodds", cat_limit = 5)
  a <- ggplot2::ggplot_build(plot(res))$layout$layout$name
  b <- ggplot2::ggplot_build(plot(res, labels = NULL))$layout$layout$name
  expect_equal(as.character(a), as.character(b))
})

test_that("the deprecated shim forwards labels identically", {
  set.seed(42)
  pd  <- make_mock_part_dta(c("bpd", "vis"))
  res <- gg_partial_varpro(pd, scale = "logodds", cat_limit = 5)
  shim <- res
  class(shim) <- c("gg_partialpro", "list")
  p <- plot(shim, labels = c(bpd = "BP Diastole"))
  strips <- as.character(ggplot2::ggplot_build(p)$layout$layout$name)
  expect_true("BP Diastole" %in% strips)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test_gg_partial_varpro.R")'`
Expected: FAIL — `unused argument (labels = ...)`

- [ ] **Step 3: Add the argument and the labeller**

In `R/plot.gg_partial_varpro.R`, change the signature:

```r
plot.gg_partial_varpro <- function(x,
                                    type = c("parametric", "nonparametric",
                                             "causal"),
                                    labels = NULL,
                                    ...) {
```

Immediately after `ylabel <- .partial_varpro_ylabel(prov)`, add:

```r
  ## Labels are a presentation concern: resolved here and applied to the facet
  ## strips, never written back into x.  The returned object keeps raw variable
  ## names, because changing them would be a breaking change downstream.
  lab_lookup <- .forest_labels(labels)
  strip_labeller <- ggplot2::as_labeller(
    function(v) .apply_forest_labels(v, lab_lookup)
  )
```

Then change **both** `facet_wrap` calls in this function from:

```r
      ggplot2::facet_wrap(~name, scales = "free_x") +
```

to:

```r
      ggplot2::facet_wrap(~name, scales = "free_x", labeller = strip_labeller) +
```

Add the roxygen parameter above the function, matching the file's Rd-macro style:

```r
#' @param labels Optional variable labels for the facet strips.  One of: a named
#'   character vector (\code{c(bpd_last = "BP Diastole")}); a labelled data frame,
#'   whose \code{attr(col, "label")} values are read; or a two-column
#'   \code{key}/\code{label} data frame.  Variables with no label keep their raw
#'   name.  Defaults to \code{NULL} (raw names).
```

- [ ] **Step 4: Forward through the shims**

In `R/plot.gg_partial.R`, replace `plot.gg_partialpro()`:

```r
plot.gg_partialpro <- function(x, type = c("parametric", "nonparametric",
                                            "causal"), labels = NULL, ...) {
  ## Deprecated class shim: re-dispatch to plot.gg_partial_varpro.
  class(x) <- c("gg_partial_varpro", setdiff(class(x), "gg_partialpro"))
  plot.gg_partial_varpro(x, type = type, labels = labels, ...)
}
```

In `R/autoplot_methods.R`, replace both partialpro autoplot methods:

```r
#' @rdname autoplot.gg
#' @export
autoplot.gg_partialpro <- function(object, ...) {
  ## Deprecated-class shim: re-dispatch to autoplot.gg_partial_varpro.
  class(object) <- c("gg_partial_varpro", setdiff(class(object), "gg_partialpro"))
  autoplot.gg_partial_varpro(object, ...)
}

#' @rdname autoplot.gg
#' @export
autoplot.gg_partial_varpro <- function(object, ...) {
  plot(object, ...)
}
```

These already pass `...` through, so `labels =` reaches `plot()` unchanged; no signature edit is needed there. Verify with the shim test in Step 1.

- [ ] **Step 5: Document, run tests, lint, commit**

```bash
cd ~/Documents/GitHub/ggRandomForests-labels
Rscript -e 'devtools::document()'
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'
Rscript -e 'print(lintr::lint_package())'
git add R/plot.gg_partial_varpro.R R/plot.gg_partial.R R/autoplot_methods.R man NAMESPACE tests/testthat/test_gg_partial_varpro.R
git commit -m "feat: add labels= to plot.gg_partial_varpro() and its shims"
```

---

## Task 6: `labels =` on `plot.gg_partial()` (rfsrc partial)

**Files:**
- Modify: `R/plot.gg_partial.R` — `plot.gg_partial()`
- Test: `tests/testthat/test_gg_partial.R` (append)

**Interfaces:**
- Consumes: `.forest_labels()`, `.apply_forest_labels()` from Task 1.
- Produces: `plot.gg_partial(x, labels = NULL, ...)`.

**Note:** this method's `@param ...` currently reads "Not currently used; reserved for future arguments." Leave that line alone; add a separate `@param labels`.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test_gg_partial.R`:

```r
test_that("plot.gg_partial labels facet strips", {
  gg_dta <- list(
    continuous = data.frame(x = rep(1:5, 2),
                            yhat = rnorm(10),
                            name = rep(c("bpd", "vis"), each = 5),
                            stringsAsFactors = FALSE),
    categorical = NULL
  )
  class(gg_dta) <- c("gg_partial", "list")
  p <- plot(gg_dta, labels = c(bpd = "BP Diastole"))
  strips <- as.character(ggplot2::ggplot_build(p)$layout$layout$name)
  expect_true(all(c("BP Diastole", "vis") %in% strips))
})

test_that("plot.gg_partial layers still carry data when labelled", {
  gg_dta <- list(
    continuous = data.frame(x = rep(1:5, 2),
                            yhat = rnorm(10),
                            name = rep(c("bpd", "vis"), each = 5),
                            stringsAsFactors = FALSE),
    categorical = NULL
  )
  class(gg_dta) <- c("gg_partial", "list")
  built <- ggplot2::ggplot_build(plot(gg_dta, labels = c(bpd = "BP Diastole")))
  expect_gt(nrow(built$data[[1]]), 0L)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test_gg_partial.R")'`
Expected: FAIL — `unused argument (labels = ...)`

- [ ] **Step 3: Write the implementation**

In `R/plot.gg_partial.R`, change the signature:

```r
plot.gg_partial <- function(x, labels = NULL, ...) {
  gg_dta <- x
```

After the `y_lab` block, add:

```r
  lab_lookup <- .forest_labels(labels)
  strip_labeller <- ggplot2::as_labeller(
    function(v) .apply_forest_labels(v, lab_lookup)
  )
```

Change **both** `facet_wrap` calls in this function from:

```r
      ggplot2::facet_wrap(~name, scales = "free_x") +
```

to:

```r
      ggplot2::facet_wrap(~name, scales = "free_x", labeller = strip_labeller) +
```

Add the roxygen parameter (same wording as Task 5 Step 3).

- [ ] **Step 4: Document, run tests, lint, commit**

```bash
cd ~/Documents/GitHub/ggRandomForests-labels
Rscript -e 'devtools::document()'
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'
Rscript -e 'print(lintr::lint_package())'
git add R/plot.gg_partial.R man NAMESPACE tests/testthat/test_gg_partial.R
git commit -m "feat: add labels= to plot.gg_partial()"
```

---

## Task 7: `labels =` on `plot.gg_vimp()`, deprecate `lbls`, remove the length gate

**Files:**
- Modify: `R/plot.gg_vimp.R`
- Test: `tests/testthat/test_gg_vimp.R` (append; create if absent)

**Interfaces:**
- Consumes: `.forest_labels()`, `.apply_forest_labels()` from Task 1.
- Produces: `plot.gg_vimp(x, relative, lbls, labels = NULL, ...)`.

**Two bugs and a deprecation in one task.** The current code gates labelling on `if (length(lbls) >= length(gg_dta$vars))` (`R/plot.gg_vimp.R:135`) — supply labels for *some* variables and it silently does nothing. `lbls` is deprecated in favour of `labels`; v4.0.0 is a major release, which is the correct window. Supplying both is an error.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test_gg_vimp.R` (create the file with this content if it does not exist):

```r
make_mock_vimp <- function(vars = c("bpd", "vis", "age")) {
  d <- data.frame(vars = factor(vars, levels = vars),
                  vimp = c(0.3, 0.2, 0.1),
                  positive = TRUE,
                  stringsAsFactors = FALSE)
  class(d) <- c("gg_vimp", "data.frame")
  d
}

test_that("labels rename the variable axis", {
  p <- plot(make_mock_vimp(), labels = c(bpd = "BP Diastole"))
  built <- ggplot2::ggplot_build(p)
  expect_true("BP Diastole" %in% as.character(built$layout$panel_params[[1]]$y$get_labels()))
})

test_that("a partial label set is honoured -- regression for the length gate", {
  # Three variables, ONE label. The old code silently applied nothing.
  p <- plot(make_mock_vimp(), labels = c(bpd = "BP Diastole"))
  labs_out <- as.character(ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$get_labels())
  expect_true("BP Diastole" %in% labs_out)
  expect_true("vis" %in% labs_out)
})

test_that("lbls still works but warns about deprecation", {
  expect_warning(plot(make_mock_vimp(), lbls = c(bpd = "BP Diastole")),
                 "deprecated")
})

test_that("supplying both lbls and labels is an error", {
  expect_error(plot(make_mock_vimp(),
                    lbls = c(bpd = "A"), labels = c(bpd = "B")),
               "both")
})
```

If the axis-label accessor differs in the installed ggplot2, substitute
`expect_true("BP Diastole" %in% as.character(unlist(built$layout$panel_params[[1]])))`
and note the substitution in the commit message.

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test_gg_vimp.R")'`
Expected: FAIL — `unused argument (labels = ...)`

- [ ] **Step 3: Write the implementation**

In `R/plot.gg_vimp.R`, change the signature:

```r
plot.gg_vimp <- function(x, relative, lbls, labels = NULL, ...) {
```

Replace the whole `if (!missing(lbls)) { ... }` block (lines ~132-145) with:

```r
  ## 'lbls' is deprecated in favour of 'labels' (v4.0.0).  The old argument took
  ## a named character vector only, and silently did nothing unless at least as
  ## many labels as variables were supplied; 'labels' honours a partial lookup
  ## and falls back per variable.
  if (!missing(lbls)) {
    if (!is.null(labels)) {
      stop("plot.gg_vimp: supply either 'lbls' or 'labels', not both. ",
           "'lbls' is deprecated; use 'labels'.", call. = FALSE)
    }
    warning("plot.gg_vimp: 'lbls' is deprecated and will be removed in a ",
            "future release; use 'labels' instead.", call. = FALSE)
    labels <- lbls
  }

  lab_lookup <- .forest_labels(labels)
  if (!is.null(lab_lookup)) {
    gg_plt <- gg_plt +
      ggplot2::scale_x_discrete(
        labels = function(v) .apply_forest_labels(v, lab_lookup)
      )
  }
```

Update the roxygen: keep `@param lbls` but mark it deprecated, and add `@param labels`:

```r
#' @param lbls \emph{Deprecated} as of v4.0.0; use \code{labels}.  A named
#'   character vector of alternative variable labels.
#' @param labels Optional variable labels for the variable axis.  One of: a named
#'   character vector (\code{c(bpd_last = "BP Diastole")}); a labelled data frame,
#'   whose \code{attr(col, "label")} values are read; or a two-column
#'   \code{key}/\code{label} data frame.  Variables with no label keep their raw
#'   name.  Defaults to \code{NULL} (raw names).
```

- [ ] **Step 4: Document, run tests, lint, commit**

```bash
cd ~/Documents/GitHub/ggRandomForests-labels
Rscript -e 'devtools::document()'
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'
Rscript -e 'print(lintr::lint_package())'
git add R/plot.gg_vimp.R man NAMESPACE tests/testthat/test_gg_vimp.R
git commit -m "feat: add labels= to plot.gg_vimp(), deprecate lbls, honour partial label sets"
```

---

## Task 8: `labels =` on `plot.gg_varpro()`

**Files:**
- Modify: `R/plot.gg_varpro.R` — `plot.gg_varpro()` and `.plot_varpro_main()`
- Test: `tests/testthat/test_gg_varpro.R` (append)

**Interfaces:**
- Consumes: `.forest_labels()`, `.apply_forest_labels()` from Task 1.
- Produces: `plot.gg_varpro(x, type, labels = NULL, ...)`; `.plot_varpro_main(x, type, prov, lab_lookup = NULL)`.

**Note:** this plot maps `aes(x = .data[["variable"]])` and then `coord_flip()`s, so the variable names are on a **discrete x scale**. Use `scale_x_discrete(labels = ...)`, exactly as in Task 7 — not a labeller.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test_gg_varpro.R`:

```r
test_that("plot.gg_varpro labels the variable axis", {
  skip_if_not(exists("make_mock_gg_varpro"), "no gg_varpro fixture available")
  obj <- make_mock_gg_varpro()
  p <- plot(obj, labels = c(bpd = "BP Diastole"))
  built <- ggplot2::ggplot_build(p)
  expect_true(any(grepl("BP Diastole",
                        unlist(lapply(built$layout$panel_params,
                                      function(pp) as.character(pp$y$get_labels()))))))
})
```

If `tests/testthat/helper-varpro-fixtures.R` does not already provide a
`gg_varpro` fixture, add one there first:

```r
make_mock_gg_varpro <- function(vars = c("bpd", "vis", "age")) {
  imp <- data.frame(variable = factor(vars, levels = vars),
                    z = c(2.1, 1.4, 0.5),
                    selected = c(TRUE, TRUE, FALSE),
                    stringsAsFactors = FALSE)
  out <- list(imp = imp, stats = NULL, imp.tree = NULL, conditional = NULL)
  class(out) <- c("gg_varpro", "list")
  attr(out, "provenance") <- list(family = "class", cutoff = 0.79)
  out
}
```

Adjust the fixture's element names to match what `gg_varpro()` actually returns —
run `Rscript -e 'devtools::load_all("."); str(gg_varpro(<a small fit>), max.level = 2)'`
once and mirror the real shape rather than guessing.

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test_gg_varpro.R")'`
Expected: FAIL — `unused argument (labels = ...)`

- [ ] **Step 3: Write the implementation**

In `R/plot.gg_varpro.R`, change the signature and thread the lookup through:

```r
plot.gg_varpro <- function(x, type, labels = NULL, ...) {
```

Inside `plot.gg_varpro()`, immediately before the call to `.plot_varpro_main()`, add:

```r
  lab_lookup <- .forest_labels(labels)
```

and pass it: `.plot_varpro_main(x, type, prov, lab_lookup)`.

Change `.plot_varpro_main()`'s signature:

```r
.plot_varpro_main <- function(x, type, prov, lab_lookup = NULL) {
```

and immediately before that function returns its plot object, add:

```r
  ## Variable names sit on a discrete x scale here (the plot coord_flip()s), so
  ## relabelling goes through scale_x_discrete, not a facet labeller.
  if (!is.null(lab_lookup)) {
    gg_plt <- gg_plt +
      ggplot2::scale_x_discrete(
        labels = function(v) .apply_forest_labels(v, lab_lookup)
      )
  }
```

Substitute the function's actual plot variable name for `gg_plt` if it differs —
read the function before editing.

Add the same `@param labels` roxygen block as in Task 7.

- [ ] **Step 4: Document, run tests, lint, commit**

```bash
cd ~/Documents/GitHub/ggRandomForests-labels
Rscript -e 'devtools::document()'
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'
Rscript -e 'print(lintr::lint_package())'
git add R/plot.gg_varpro.R man NAMESPACE tests/testthat/test_gg_varpro.R tests/testthat/helper-varpro-fixtures.R
git commit -m "feat: add labels= to plot.gg_varpro()"
```

---

## Task 9: NEWS, full check, and PR

**Files:**
- Modify: `NEWS.md`

- [ ] **Step 1: Append the NEWS bullets**

Add to the **end** of the `ggRandomForests v4.0.0 (development)` section — at the end, so the parallel RHF session's concurrent NEWS edit is a one-line conflict rather than a tangled one:

```markdown
* `plot.gg_partial_varpro()`, `plot.gg_partial()`, `plot.gg_vimp()` and
  `plot.gg_varpro()` gain a `labels` argument for human-readable variable names.
  It accepts a named character vector, a labelled data frame (reading
  `attr(col, "label")`), or a two-column `key`/`label` data frame. Variables with
  no label keep their raw name. The plotted object is unchanged — it still carries
  raw variable names, so downstream consumers are unaffected.
* `plot.gg_vimp()`: `lbls` is **deprecated** in favour of `labels` and will be
  removed in a future release. It also no longer requires at least as many labels
  as variables; a partial label set is now honoured, falling back per variable.
  Previously supplying fewer labels than variables silently applied none.
* `gg_partial_varpro()` orders variables by varPro importance
  (`varPro::get.topvars()`) when `object` is supplied, and `name` is now a
  **factor**, so facets follow importance order instead of being re-sorted
  alphabetically. Variables absent from the ranking keep their incoming order and
  are appended after the ranked block; none are dropped.
* `gg_partial_varpro()`: **`nvars` now selects the top n by importance.** It
  previously took the first n elements of the partial-dependence list before any
  ranking, which returned an arbitrary subset with no symptom in the output.
* `gg_partial_varpro()` warns when `scale = "auto"` cannot be resolved because no
  `object` was supplied, instead of silently falling back to the generic
  "Partial Effect" axis.
```

- [ ] **Step 2: Full check**

```bash
cd ~/Documents/GitHub/ggRandomForests-labels
Rscript -e 'devtools::document()'
Rscript -e 'print(lintr::lint_package())'
Rscript -e 'devtools::check(args = c("--no-manual"), vignettes = FALSE)'
```

Expected: `lint_package()` returns zero; `check()` reports 0 errors, 0 warnings, 0 notes.

- [ ] **Step 3: Commit and open the PR**

```bash
cd ~/Documents/GitHub/ggRandomForests-labels
git add NEWS.md man NAMESPACE
git commit -m "docs: NEWS for labels, importance ordering, and the nvars fix"
git push -u origin feat/forest-plot-labels
gh pr create --base main \
  --title "Labelled, importance-ordered forest plots" \
  --body "See dev/plans/2026-08-28-forest-plot-labels-design.md.

Fixes three silent defects and adds variable labels to four plot methods.

- \`nvars\` selected the *first* n variables rather than the top n by importance, with no symptom in the output.
- Facets were re-sorted alphabetically because \`name\` was a character column, discarding varPro importance order.
- \`plot.gg_vimp(lbls =)\` silently applied nothing unless at least as many labels as variables were supplied.

Also adds a warning when \`scale = \"auto\"\` cannot resolve for want of a fit, and deprecates \`lbls\` in favour of \`labels\`.

No version bump — lands on the v4.0.0 development line for RC2.

RHF importance sites are deliberately excluded; a parallel session owns those files.

🤖 Generated with [Claude Code](https://claude.com/claude-code)"
```

- [ ] **Step 4: Resolve Copilot review threads**

`main` is protected by the `protect main` ruleset, which runs an automatic Copilot review on every PR. Unresolved threads no longer block the merge, so nothing forces the feedback to be read — address and resolve the threads before handing the PR over.

---

## Notes for the implementer

- **Do not "fix" the categorical/continuous split.** `plot.gg_partial_varpro()` returns a `patchwork` of two plots, so importance ordering cannot interleave categorical and continuous variables. This is known, documented in the design, and deferred. ggplot2 rejects mixing a numeric and a discrete x scale in one `facet_wrap` ("Discrete value supplied to a continuous scale"), so the obvious unification does not work — do not start down that path.
- **Do not annotate the three-series overlay.** Also deferred.
- **Do not touch RHF files.** A parallel session owns them.
- If a test in the existing suite breaks because `name` is now a factor, that is the intended new contract — update the test with `as.character()` rather than reverting the factor.
