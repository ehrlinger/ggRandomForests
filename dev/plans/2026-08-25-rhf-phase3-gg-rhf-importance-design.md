# RHF Phase 3 (`gg_rhf_importance`) Design

**Date:** 2026-08-25

**Status:** Approved in conversation; implementation not started

**Target:** `dev_rhf`

**Scope:** RHF Phase 3 only

## Purpose

`randomForestRHF::importance.rhf()` calculates a variable-priority curve for
each predictor over the RHF evaluation grid. Phase 3 adds the
`ggRandomForests` extraction and plotting layer for that result:

- `gg_rhf_importance()` returns the upstream result as a tidy data frame;
- `plot.gg_rhf_importance()` draws the time-by-variable point matrix;
- `print()`, `summary()`, and `autoplot()` follow the rest of the `gg_*`
  family.

The extractor takes a fitted `rhf` object. It calculates
`importance.rhf()` when needed, but the documentation leads with a supplied
`importance_fit` because cache construction and the full window sweep are
expensive.

This phase remains a visualization layer. It does not fit RHF models, rebuild
the upstream priority calculation, or attach `randomForestRHF`.

## Method contract

The Phase 3 terminology follows Ishwaran, Hsich, Kogalur, and Lee (2026), not
the preliminary wording in the May umbrella design.

For a forest rule, releasing predictor `v` removes that predictor's split
constraints while leaving the other constraints in place. The released region
provides a near-miss set. Within a time window, RHF compares the log integrated
hazard exposure for intervals inside the rule with the corresponding near
misses. The variable-priority score is the average absolute contrast over
eligible rules.

Consequences for the public API:

- The tidy value is named `priority`, faithfully mapping the upstream
  `importance` column.
- It is not called a z-score. Although an internal upstream workhorse uses a
  column named `z`, the published RHF estimand is a variable-priority score.
- There is no `selected` column or `0.79` cutoff. The RHF paper and
  `importance.rhf()` do not define that threshold for this time-local score.
- Documentation describes larger scores as larger local rule-release
  contrasts. It does not call them p-values, probabilities, or thresholded
  statistical significance.
- A single global ranking is a display aid. The time profile is the result:
  variables may remain prominent throughout follow-up or concentrate in early
  or late windows.

## Dependency and citation contract

Phase 3 requires the current CRAN API:

```text
Suggests:
    randomForestRHF (>= 1.0.1)
```

The implementation must remain guarded with
`requireNamespace("randomForestRHF", quietly = TRUE)`. No package from
`Suggests` is attached from `R/`.

The RHF help pages cite both the method and the software:

1. Ishwaran H, Hsich EM, Kogalur UB, Lee DKK (2026). “Random Hazard
   Forests.” arXiv:2608.21597. doi:10.48550/arXiv.2608.21597.
2. Ishwaran H, Kogalur UB (2026). *randomForestRHF: Random Hazard Forests*.
   R package version 1.0.1.

The method citation is added to `gg_rhf()`, `gg_auct()`, and
`gg_rhf_importance()`. The software citation is included in the new Phase 3
help page and the Phase 5 RHF vignette bibliography. The eventual vignette
uses the paper as the main source for predictable covariate paths,
no-lookahead routing, pathwise hazard estimation, cumulative and incident
AUC, and time-localized variable priority.

## Public interface

```r
gg_rhf_importance(object, ...)

gg_rhf_importance.rhf(
  object,
  importance_fit = NULL,
  cache = NULL,
  time.index = NULL,
  ...
)
```

### Calculation path

When `importance_fit` is `NULL`, the method calls:

```r
randomForestRHF::importance.rhf(
  o = object,
  cache = cache,
  time.index = time.index,
  ...
)
```

This keeps `trim`, `max.rules.tree`, `max.tree`, `eps`, `y.external`, and
`verbose` upstream rather than copying their interface into
`ggRandomForests`.

### Precomputed path

When `importance_fit` is supplied, it must inherit from `"importance.rhf"`.
The extractor uses it directly and records `precomputed = TRUE` in
provenance. The help page leads with this form:

```r
cache <- randomForestRHF::varpro.cache.rhf(rhf_fit)
imp <- randomForestRHF::importance.rhf(rhf_fit, cache = cache)
gg_imp <- gg_rhf_importance(rhf_fit, importance_fit = imp)
```

`cache`, `time.index`, and calculation arguments in `...` are meaningful only
when `importance_fit` is `NULL`. Supplying both a precomputed result and
calculation arguments is an error rather than a silently ignored request.

### Alignment validation

A supplied result must be compatible with `object`:

- `importance_fit$xvar.names` must match `object$xvar.names`;
- `importance_fit$importance.matrix` must be a nonempty numeric matrix with
  those variables as rows;
- `importance_fit$window.info` must contain `index`, `time`, `start`, `stop`,
  `midpoint`, `n.risk`, `n.rules`, and `label`;
- the matrix columns, `window.info` rows, and `importance.long` windows must
  align;
- window indices must fall within `object$time.interest`, and their time
  values must match the corresponding RHF grid values within numeric
  tolerance.

An incompatibility raises a specific error before any tidy frame is built.

## Returned object

The extractor returns a `data.frame` with class:

```r
c("gg_rhf_importance", "data.frame")
```

The columns are:

| Column | Type | Meaning |
|---|---|---|
| `variable` | factor | Predictor name. Levels run least to most important by q90 priority, placing the leading variable at the top of the plot. |
| `time_window` | character | Upstream interval label, such as `(733, 859]`. |
| `time` | numeric | Right endpoint of the evaluation-grid window. |
| `time_index` | integer | Index into `object$time.interest`. |
| `start` | numeric | Left endpoint of the window. |
| `stop` | numeric | Right endpoint of the window. |
| `midpoint` | numeric | Window midpoint. |
| `n_risk` | integer | Number at risk in the window. |
| `n_rules` | integer | Number of active rules used in the window calculation. |
| `priority` | numeric | Time-localized RHF variable-priority score. |

Names use the package's snake-case convention while preserving every field in
the upstream `importance.long` table. Original priority values are never
capped or transformed in the returned object.

Rows retain the upstream order: chronological window blocks, with variables
in decreasing priority within each block. Reversing factor levels for plotting
must not reorder the public data frame.

### Attributes

The regular RHF provenance from `.set_provenance()` is extended with:

- `precomputed`: whether `importance_fit` was supplied;
- `y_source`: upstream working-response source;
- `trim`: upstream winsorized aggregation setting;
- `n_windows`: number of analyzed windows;
- `rank_by`: `"q90"`;
- `randomForestRHF_version`: installed package version used for extraction,
  or `NA` when a saved precomputed result is extracted without the suggested
  package installed.

No upstream cache or `importance.rhf` object is retained on the result. Callers
who need those objects keep them explicitly.

## Variable ordering

For each variable, compute the 90th percentile of its finite priority scores
over analyzed windows. Ties break by median priority, maximum priority, then
variable name. This matches the upstream default emphasis on q90 while making
the order deterministic.

The long frame keeps its chronological row order. Only the factor levels carry
the display order, with the highest-ranked variable as the last level. This
extends the importance-plot convention already pinned across `gg_vimp`,
`gg_varpro`, `gg_beta_varpro`, and `gg_ivarpro`.

## Plot contract

```r
plot.gg_rhf_importance(
  x,
  vars = NULL,
  top_n_union = 15L,
  transform = c("none", "log10"),
  size_cap = 0.99,
  color_cap = 0.99,
  display_note = TRUE,
  ...
)
```

The default is a ggplot2 point matrix:

- x-axis: ordered `time_window` values;
- y-axis: `variable`, with the highest q90 priority at the top;
- point size: displayed priority magnitude;
- point color: displayed priority magnitude;
- theme: `theme_bw()` with light panel guides;
- legend title: `RHF variable priority`.

This mirrors the published barplot matrix and the upstream
`dotmatrix.importance.rhf()` while returning a ggplot object.

When `vars` is supplied, only those variables are shown and unknown names are
an error. Otherwise, each window contributes its leading `top_n_union`
variables and the plot displays their union. `top_n_union = NULL` displays all
variables. The displayed rows retain the global q90 factor order.

`transform = "log10"` applies `log10(priority + 1)` for display only. Size and
color caps are quantiles of the finite display values. Capping never changes
the object, and a caption reports an applied cap when `display_note = TRUE`.
Zero scores remain visible as the smallest point; missing scores are not
drawn. If no finite scores remain after variable filtering, the method errors
instead of returning an empty plot.

The default method returns one `ggplot` object and never prints it.

## Print, summary, and autoplot

`print.gg_rhf_importance()` uses `.gg_header()` and reports:

- number of variables and windows;
- working-response source;
- whether the result was precomputed;
- q90 as the ranking rule.

It returns the object invisibly.

`summary.gg_rhf_importance()` returns one row per variable, sorted from most
to least important, with:

```text
variable / q90 / median / mean / max / n_windows / n_finite
```

The summary contains no selection flag. `autoplot.gg_rhf_importance()` simply
returns `plot(object, ...)`.

## Error handling

The implementation fails plainly for:

- a non-`rhf` input;
- missing `randomForestRHF` when calculation is requested;
- a supplied object that is not `importance.rhf`;
- a supplied result that does not align with the RHF predictors or time grid;
- malformed or empty upstream matrices/tables;
- simultaneous precomputed and calculation-only arguments;
- invalid `vars`, `top_n_union`, transformation, or cap values;
- a plot request with no finite priority values.

Upstream calculation errors propagate without being rewritten, so users can
identify `randomForestRHF::importance.rhf()` as their source.

## Documentation scope

Phase 3 changes user-facing documentation in these places:

- new `gg_rhf_importance()` and `plot.gg_rhf_importance()` help pages;
- RHF paper references in `gg_rhf()` and `gg_auct()`;
- a Phase 3 NEWS entry;
- `_pkgdown.yml` survival-analysis reference entries;
- the May umbrella design, updated to replace `z / selected / heatmap` with
  `priority / point matrix` and to record the compute-or-reuse interface;
- `vignettes/ggRandomForests.bib`, with method and software entries ready for
  the Phase 5 RHF vignette.

Documentation explains inherited upstream behavior and leads with the cached,
precomputed workflow. Examples use public package data and remain inside
`\donttest{}` because RHF fitting and priority calculation are slow.

### Phase 5 vignette follow-up

- [ ] At the RHF Phase 5 documentation pass, create a dedicated RHF vignette
  if the package still has none. If an RHF vignette has been added before
  then, update it instead. The vignette must cover the fitted-object workflow,
  `gg_rhf()`, `gg_auct()`, `gg_rhf_importance()`, and the Phase 4 tuning
  wrapper; lead with precomputed importance reuse; explain predictable
  covariate paths and no-lookahead routing; distinguish cumulative from
  incident AUC; interpret time-localized priority without a significance
  cutoff; and cite both the 2026 RHF paper and `randomForestRHF` software.

## Test design

Most tests use a small constructed `importance.rhf` object. This keeps unit
tests fast and lets them exercise malformed shapes, ordering, transformations,
and missing values without fitting a forest.

One session-memoized integration fixture extends `helper-rhf-fixtures.R`:

- reuse `.rhf_pbc()`;
- build one `varpro.cache.rhf` cache with deliberately small rule/tree limits;
- calculate a small, deterministic subset of time windows;
- call `set.seed()` inside every `test_that()` block that touches the RNG.

Coverage includes:

1. compute and precomputed paths produce the same tidy values;
2. exact column names, classes, row order, factor order, and provenance;
3. `priority` equals upstream `importance` without transformation;
4. the precomputed path does not call the upstream calculator;
5. predictor and time-grid mismatches fail;
6. malformed and empty upstream objects fail;
7. q90 ordering and deterministic tie-breaking;
8. explicit `vars`, top-variable union, all-variable display, log transform,
   display-only capping, zero values, and missing values;
9. `plot()` and `autoplot()` return ggplot objects;
10. `print()` returns invisibly and `summary()` returns the documented frame;
11. the cross-family most-important-at-top convention includes
    `gg_rhf_importance`;
12. one vdiffr point-matrix baseline in `test_snapshots.R`.

Slow integration and snapshot blocks call `skip_on_cran()` and
`skip_if_not_installed("randomForestRHF")`. Snapshot runs retain the repository
requirement `VDIFFR_RUN_TESTS=true`.

## Files in implementation scope

Expected source and test changes:

- create `R/gg_rhf_importance.R`;
- create `R/plot.gg_rhf_importance.R`;
- create `tests/testthat/test_gg_rhf_importance.R`;
- create `tests/testthat/test_plot_gg_rhf_importance.R`;
- modify `R/gg_rhf.R`, `R/gg_auct.R`, `R/print_methods.R`,
  `R/summary_methods.R`, and `R/autoplot_methods.R`;
- modify `tests/testthat/helper-rhf-fixtures.R`,
  `tests/testthat/test_plot_conventions.R`, and
  `tests/testthat/test_snapshots.R`;
- modify `DESCRIPTION`, `NEWS.md`, `_pkgdown.yml`,
  `vignettes/ggRandomForests.bib`, and the RHF umbrella design;
- regenerate `NAMESPACE` and `man/` with `devtools::document()`;
- add the generated vdiffr SVG baseline.

No new package dependency is introduced. The existing suggested dependency
receives an explicit minimum version. The package remains at `4.0.0` during
the unfinished v4 cycle.

## Definition of done

Implementation is complete only after the repository gates pass in order:

```bash
Rscript -e 'devtools::document()'
Rscript -e 'lintr::lint_package()'
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::test()'
```

`git status` is checked before and after the suite so snapshot deletion cannot
be mistaken for an intended change. `R CMD check --as-cran` with the manual is
run once for the eventual PR after implementation and review.

## References

- Ishwaran H, Hsich EM, Kogalur UB, Lee DKK (2026). “Random Hazard
  Forests.” arXiv:2608.21597. <https://doi.org/10.48550/arXiv.2608.21597>.
- Ishwaran H, Kogalur UB (2026). *randomForestRHF: Random Hazard Forests*.
  R package version 1.0.1.
  <https://CRAN.R-project.org/package=randomForestRHF>.
- Lu M, Ishwaran H (2024). “Model-independent variable selection via the
  rule-based variable priority framework.” arXiv:2409.09003.
