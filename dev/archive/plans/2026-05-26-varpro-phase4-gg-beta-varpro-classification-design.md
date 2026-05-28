# ggRandomForests v2.8.0 — varPro Phase 4c-classification: `gg_beta_varpro` for `class` family

**Date:** 2026-05-26
**Author:** John Ehrlinger (design via Claude brainstorming)
**Status:** Approved — ready for implementation planning
**Sequencing:** Sibling sub-project to Phase 4c (`gg_beta_varpro`, regression-only, PR #97 merged at `2.7.3.9011`). Lands as one PR; motivating use case is **30-day mortality** (binary clinical outcome).

---

## Goal

Extend `gg_beta_varpro()` to handle `varPro::varpro` classification forests (binary and multi-class) by family auto-dispatch, with a clinician-friendly default for binary fits (single positive-class panel) and a faceted view for multi-class fits.

## Scope

Single PR. Adds `class` family handling alongside the existing `regr` family. `regr+` and `surv` remain deferred.

---

## What `beta.varpro` returns for `class` family

Empirically confirmed against `varPro 3.1.0` on `iris` (multi-class) and `iris[!=setosa]` (binary):

| Family | `$results` columns |
|---|---|
| `regr` | `tree, branch, variable, n.oob, imp` |
| `class` (K=2) | `tree, branch, variable, n.oob, n.oob.1, n.oob.2, imp, imp.1, imp.2` |
| `class` (K>2) | `tree, branch, variable, n.oob, n.oob.1..K, imp, imp.1..K` |

The `imp` column on classification is the **sum of |β̂| across classes** (per the `beta.varpro` source: `beta_out[k, ] <- c(sum(b_class, na.rm = TRUE), b_class)`). Each `imp.<k>` column is the lasso β̂ for predicting class k inside that rule's region. The `n.oob.<k>` columns are class-stratified OOB counts.

Binary and multi-class share this schema; only K differs. The wrapper treats them symmetrically.

---

## Architecture

```
varPro::varpro fit (regr or class)  ──►  gg_beta_varpro(fit, ..., cutoff = NULL,
                                                       beta_fit = NULL,
                                                       which_class = NULL)
                                                       │
                                                       ├── family dispatch
                                                       │     ├─ regr  → existing path (unchanged shape)
                                                       │     └─ class → new long-format path
                                                       │
                                                       └── tidy data.frame
                                                           class: c("gg_beta_varpro", "data.frame")
                                                           cols : variable (factor),
                                                                  [class],          ← class path only
                                                                  beta_mean,
                                                                  n_rules,
                                                                  selected
                                                                  │
                                                           plot / print / summary / autoplot
```

Single S3 class `gg_beta_varpro`. The plot method branches on the presence of a `class` column, just like `plot.gg_roc` branches on the presence of a `class` column for per-class ROC. No new exported function.

---

## Signature

```r
gg_beta_varpro(object, ..., cutoff = NULL, beta_fit = NULL, which_class = NULL)
```

- **`object`** — `varpro` fit, family `regr` or `class`. Other families error.
- **`...`** — forwarded to `varPro::beta.varpro()` when `beta_fit = NULL`; ignored otherwise (one warning).
- **`cutoff`** — selection threshold(s). See "Cutoff polymorphism" below.
- **`beta_fit`** — pre-computed `varPro::beta.varpro()` result (cache path, unchanged from Phase 4c).
- **`which_class`** — for `class` family only:
  - `NULL` (default): binary → resolves to the last factor level (positive class); multi-class → `NULL` stays (all classes, faceted).
  - String matching a level of the response factor → single panel for that class.
  - String not in `levels(yvar)` → error `"which_class = '<x>' is not a level of the response. Levels: <a, b, ...>."`
  - On a regression fit, supplying `which_class` warns and is ignored.

### Cutoff polymorphism

`cutoff` accepts three shapes:

| Input | Resolution |
|---|---|
| `NULL` (default) | Per-class `mean(beta_mean)` within each class. For regression, `mean(beta_mean)` across variables (a single value). |
| Numeric scalar | Applied uniformly across every class (regression: same as today). |
| Named numeric vector (names ⊆ class levels) | Per-class override. Missing names fall back to that class's `mean(beta_mean)`. Unknown names error with the list of valid levels. |

### Sort key (factor-level ordering across panels)

Variables are stored as a **factor** whose levels are set by `mean(|imp_total|)` descending — i.e. the per-variable aggregate over `beta.varpro`'s `imp` column (which is sum-across-classes for `class` family). One ordering, every facet uses it, the single-panel binary view uses it too. The data frame's row order also follows this for stability across `dplyr` / `tidyr` operations that strip factor levels.

For regression, the factor-level ordering and the `beta_mean` ordering coincide (`imp_total == imp` when K=1).

This is the **package-wide convention going forward** — see Out-of-scope alignment follow-up below.

---

## Provenance — shape alignment across families

The Phase 4c provenance stored `cutoff` as a scalar. To make the contract consistent across families and stable for downstream tooling, the classification path stores `cutoff` as a **named numeric vector** with names equal to class levels. **The regression path also moves to a named vector**, length 1, named `"regr"`. This is a small breaking change in provenance shape — acceptable because the field is internal (none of the regression Phase 4c tests assert on `prov$cutoff[1]` specifically; one test asserts `prov$cutoff` equals `mean(beta_mean)`, which still works on a 1-element vector via implicit conversion, but the test needs an update to use `prov$cutoff[["regr"]]` for clarity).

Full provenance for the classification path:

- `source = "varPro::beta.varpro"`
- `family = "class"`
- `ntree`
- `cutoff` — named numeric vector, one entry per class level
- `cutoff_default` — `TRUE` when user passed `NULL`
- `use.cv` — `TRUE` / `FALSE` / `NA` (NA when `beta_fit` is supplied)
- `n_rules_total`
- `n_rules_nonzero` — sum across all per-class columns
- `precomputed` — `!is.null(beta_fit)`
- `xvar.names`
- `class_levels` — character vector (omitted for regression)
- `which_class` — string for single-panel mode, `NULL` for faceted mode (omitted for regression)

---

## Internal flow

1. Validate family: `regr` or `class`. Anything else errors with the deferred-work signpost (existing message, slightly broadened).
2. If `which_class` is supplied with a regression fit: `warning("which_class ignored for regression family.")`; set internal `which_class <- NULL`.
3. Resolve `beta_fit` (same as Phase 4c).
4. If `family == "regr"`: run the existing aggregation; wrap `cutoff` as `c("regr" = resolved_cutoff)` in provenance; return.
5. If `family == "class"`:
   a. `class_levels <- levels(object$y)` (or `colnames` of the appropriate matrix — verify against varPro's representation; see Step 5 of implementation plan).
   b. `imp_cols <- paste0("imp.", seq_along(class_levels))`.
   c. For each class `k`:
      - `res_k <- b$results[, c("variable", imp_cols[k])]`; filter finite, drop NA.
      - For each released variable in `res_k`: `beta_mean[v, k] = mean(|res_k$imp.<k>|)`; `n_rules[v, k] = nrow(res_k for v)`.
   d. Compute `imp_total_per_variable = mean(|b$results$imp|)` over released variables (used for factor-level ordering).
   e. Resolve cutoffs per class (NULL → per-class mean; scalar → broadcast; named vector → per-class with fallback).
   f. Build long-format frame `(variable, class, beta_mean, n_rules, selected)`.
   g. Set `variable` as factor with levels ordered by descending `imp_total_per_variable`.
   h. If `which_class` is non-NULL or binary-default-resolved: filter to that class.
   i. Sort rows for stability: by `class` (factor level order), then `variable` (factor level order).
6. Attach provenance, set class.

---

## Plot

`plot.gg_beta_varpro` already exists from Phase 4c. Extend it:

- If `class` column is **absent**: existing single-panel bar chart (regression). Unchanged.
- If `class` column is **present**:
  - If only one class is in the data (binary default-positive-class or `which_class` set): single bar chart, same look as regression. Caption notes the class.
  - If multiple classes: `facet_wrap(~ class)` with per-class cutoff lines (`geom_hline(yintercept = cutoff[class])` via class-keyed data join, not a single vline). Variables share factor levels across facets.

Per-class cutoff line in the faceted plot uses a one-row-per-facet data frame:

```r
hline_df <- data.frame(class = names(cutoff_vec), cutoff = unname(cutoff_vec))
... + geom_hline(data = hline_df, aes(yintercept = cutoff), linetype = "dashed", color = "#e74c3c")
```

Bars filled by `selected` (blue / grey) as in Phase 4c. Caption surfaces `n_rules_total`, `use.cv`, and either the resolved class name (single panel) or `"<K> classes"` (faceted).

---

## S3 companions

- **`print.gg_beta_varpro`** (already lives in `R/print_methods.R`). Extend the suffix to handle the classification case:
  - `n_classes: K  |  which_class: <name>  |  precomputed: <bool>` (single panel)
  - `n_classes: K  |  view: faceted  |  precomputed: <bool>` (faceted)
  - Footer line: `"<sel_total> of <n_var × K> (variable, class) pairs selected"`.
- **`summary.gg_beta_varpro`** (already lives in `R/summary_methods.R`):
  - Regression (unchanged): named numeric vector descending, `n_rules` attribute.
  - Classification: **list of per-class summaries**, each a named numeric vector descending with its own `n_rules` attribute. Print method on `summary.gg_beta_varpro` knows to iterate when the underlying object is a list.
- **`autoplot.gg_beta_varpro`** — unchanged delegation.

---

## Tests

Mirror Phase 4c with a classification fixture pair. New tests appended to `tests/testthat/test_gg_beta_varpro.R`:

1. **Binary shape**: `gg_beta_varpro(vb, beta_fit = bb)` returns frame with `class` column, both binary levels present.
2. **Multi-class shape**: K=3 for iris, three classes present.
3. **`variable` is a factor with `imp`-descending levels**: assert `is.factor(out$variable)` and `levels(out$variable)` matches the descending `mean(|imp|)` ordering computed independently.
4. **Aggregation correctness** for at least one (variable, class) pair: `beta_mean` equals `mean(abs(b$results$imp.<k>[b$results$variable == idx]))`.
5. **Binary `which_class = NULL` default** resolves to the **last factor level**: `attr(out, "provenance")$which_class` equals `tail(levels(iris2$Species), 1)`.
6. **Multi-class `which_class = NULL` default**: provenance carries `which_class = NULL`; all K classes present in `out$class`.
7. **Explicit `which_class`**: single panel, only that class in `out$class`, provenance reflects it.
8. **`which_class` not in levels**: errors with the levels listed in the message.
9. **Per-class default cutoff**: `prov$cutoff` is a named numeric of length K; `prov$cutoff[k]` equals `mean(out$beta_mean[out$class == k])`.
10. **Scalar `cutoff`**: broadcast across classes; `prov$cutoff` is length-K with all entries equal to the scalar.
11. **Named vector `cutoff`**: respected; missing names fall back to per-class mean. Unknown names error.
12. **Regression provenance shape now named**: `prov$cutoff` is `c("regr" = X)`, length 1. Update the Phase 4c test that asserted scalar.
13. **`regr+` / `surv` still error** with the deferred-work signpost (run iff a survival fixture is cheap; otherwise mock the family field).
14. **`beta_fit` cached path on classification**: cache-equivalence held under slow-test guard.
15. **Plot smoke** — binary single panel and multi-class facet both build via `ggplot_build()`.
16. **Print + summary smoke** — classification print/summary return expected shapes.

All tests use the session-memoised classification fixtures (see Files).

---

## Snapshots

Two new `vdiffr::expect_doppelganger` baselines, both under `VDIFFR_RUN_TESTS`:

- `gg-beta-varpro-class-binary` — `iris[!=setosa]` binary fit, default plot (single positive-class panel).
- `gg-beta-varpro-class-multiclass` — `iris` 3-class fit, default plot (faceted).

---

## Documentation

- Extend the existing `gg_beta_varpro` roxygen with a new `@section Classification:` block:
  - What `imp.<k>` is (per-class lasso β̂; same pedantic-β semantics as regression, applied per class).
  - The binary default (last factor level = positive class) and `which_class` override.
  - The faceted multi-class view and per-class cutoffs.
  - One worked example showing 30-day-mortality-style usage:
    ```r
    fit <- varPro::varpro(event_30d ~ ., data = clinical, ntree = 200)
    gg  <- gg_beta_varpro(fit)         # binary → last-level panel by default
    plot(gg)
    ```
- Update `plot.gg_beta_varpro` "Reading the chart" section: mention the factor-level alignment across facets, per-class cutoff lines.
- `@note`: name the `regr+` / `surv` gap; point to the spec for the multivariate plot-story brainstorm punt.
- `_pkgdown.yml`: no new pages, but bump the function reference description if it mentions "regression only."

---

## Out of scope

- `regr+` (multivariate regression) — deferred to v2.9.0 brainstorm.
- Survival — blocked on upstream (`beta.varpro` errors for `surv`).
- `which_class` as a vector subset of classes (>1 but <K) — single class or all-K-faceted only.
- Per-class `faithful = TRUE` overlay (per-rule β jitter inside facets).
- **Consistency follow-up — separate PR**: propagate the "factor-levels-as-importance-order" pattern to `plot.gg_varpro(conditional = TRUE)` and `gg_vimp`. They currently sort by the unconditional median z or by `vimp` descending without committing to a factor-level contract. A future cleanup PR should align all three (`gg_beta_varpro`, `gg_varpro`, `gg_vimp`) to one shared sorting convention so cross-method comparisons line up row-for-row. Tracked here so it doesn't slip.

---

## Acceptance criteria

- `R CMD check --as-cran`: 0 errors / 0 warnings / 0 notes.
- `devtools::test()`: 0 failures. Phase 4c regression coverage stays green (with the one provenance-shape test update).
- `GG_BETA_VARPRO_SLOW_TESTS=true VDIFFR_RUN_TESTS=true devtools::test()`: 0 failures, both snapshots build cleanly.
- One PR before the v2.8.0 release candidate.

## Files

- **Modify**: `R/gg_beta_varpro.R` (family dispatch + class path), `R/plot.gg_beta_varpro.R` (faceted branch), `R/print_methods.R` (extend `print.gg_beta_varpro` suffix), `R/summary_methods.R` (extend `summary.gg_beta_varpro` for list-of-classes), `tests/testthat/helper-beta_varpro.R` (add `.varpro_iris_binary()`, `.varpro_iris_multiclass()`, `.beta_fit_iris_binary()`, `.beta_fit_iris_multiclass()`), `tests/testthat/test_gg_beta_varpro.R` (~16 new tests + 1 Phase 4c update), `tests/testthat/test_snapshots.R` (2 baselines), `NEWS.md`, `DESCRIPTION` (`2.7.3.9012`), `_pkgdown.yml` (reference-description tweak if needed).
- **New**: none.

---

## Sequencing note

After this lands, the only remaining planned Phase 4 sub-project is `gg_ivarpro` (Phase 4d). The factor-level-alignment consistency follow-up is a separate, smaller PR that can land in parallel or after.
