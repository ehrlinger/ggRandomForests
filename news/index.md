# Changelog

## ggRandomForests v2.8.0 (development) — continued

- [`gg_beta_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_varpro.md)
  adds varPro classification support (binary + multi-class). Binary fits
  default to a single positive-class panel (last factor level);
  multi-class fits return a long-format frame with a `class` column and
  plot as `facet_wrap(~ class)`. Optional `which_class` selects a single
  class; `cutoff` accepts a scalar or per-class named vector. Variables
  are stored as a factor whose levels are set by
  `mean(|sum-of-class-beta|)` descending so every facet shows rows in
  the same order. Motivating use case: 30-day mortality.
- Provenance shape change for
  [`gg_beta_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_varpro.md):
  `attr(*, "provenance")$cutoff` is now always a named numeric vector —
  length 1 named `"regr"` for regression, length K named with the
  response factor levels for classification. Downstream tooling should
  read it as a vector and select by name; the prior scalar shape is
  gone.
- [`gg_beta_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_varpro.md)
  and
  [`plot.gg_beta_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_beta_varpro.md):
  tidy wrapper and default horizontal bar chart for
  [`varPro::beta.varpro()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
  — the per-rule lasso-β refinement of variable importance. Aggregates
  per-rule β̂ by variable into `beta_mean = mean(|β̂|)` and flags
  variables above a selection cutoff (default `mean(beta_mean)`).
  Optional `beta_fit` argument lets callers compute the expensive
  `beta.varpro()` step once and reuse the result across multiple wrapper
  calls (different cutoffs, snapshot rebuilds, vignette knits). `print`
  / `summary` / `autoplot` S3 companions follow the existing `gg_*`
  conventions. **Regression family only** — classification, regr+, and
  survival are tracked under Phase 4d (see the spec for the endpoint
  map). Third of three Phase 4 sub-projects.
- [`gg_isopro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_isopro.md)
  gains a `newdata` argument so a fitted
  [`varPro::isopro`](https://www.randomforestsrc.org/reference/isopro.html)
  model can score new observations into the same tidy `gg_isopro` frame.
  Internally the wrapper calls `predict.isopro()` twice: with
  `quantiles = FALSE` to populate the `case.depth` column (varPro’s
  native polarity, lower = more anomalous) and with `quantiles = TRUE`
  to compute `howbad = 1 - quantile` (the wrapper convention, higher =
  more anomalous). Both polarities are visible in the returned data
  frame, and the relationship is named in the roxygen. The `plot` /
  `print` / `summary` / `autoplot` S3 companions work unchanged on the
  new tidy frame; to overlay training and test scores, bind the two
  extractor calls with a `method` label column and pass the result to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html). Second of
  three Phase 4 sub-projects.
- **Fix (gg_isopro training-path polarity).** Bug in the original
  `gg_isopro` (PR
  [\#94](https://github.com/ehrlinger/ggRandomForests/issues/94)):
  varPro’s `$howbad` on an `isopro` fit uses “lower = more anomalous”
  polarity (it is the quantile of `case.depth`), but the wrapper’s plot
  method and documentation both assume “higher = more anomalous”. Train
  scores and the new test-data scores were anti-correlated until this
  PR’s training-path flip (`howbad = 1 - object$howbad`) brought them
  into agreement. The fix surfaced because the test-data sanity check
  (training-as-newdata top-5 overlap) failed at 0/5 instead of 5/5
  before the flip. Note: the two vdiffr baselines recorded in PR
  [\#94](https://github.com/ehrlinger/ggRandomForests/issues/94)
  (`gg-isopro-default` and `gg-isopro-threshold`) were recorded under
  the inverted polarity; they are visually flipped relative to the new
  behaviour but CI skips snapshots (`VDIFFR_RUN_TESTS = false`) so no
  failure surfaces. Re-record with `VDIFFR_RUN_TESTS = true` when
  convenient.
- Documentation: pedagogical pass over the varPro wrappers
  (`gg_partial_varpro`, `gg_varpro`, `gg_udependent` and their `plot.*`
  methods). Each help page now has explicit “What X is doing”, “What’s
  in the output”, and “What you use this for” sections so a reader new
  to varPro can learn the underlying method (release rules, beta-entropy
  dependency, parametric / nonparametric / causal partial estimators)
  from the help page alone, not just the wrapper mechanics. No API or
  behavioural change.
- Documentation: enable roxygen2 markdown package-wide via
  `Roxygen: list(markdown = TRUE)` in `DESCRIPTION`. New roxygen blocks
  can use backticks and `[fn()]` link syntax; existing `\code{}` /
  `\link{}` markup keeps working. Two source-roxygen edits to keep R CMD
  check clean: `randomForest[SRC]` in `R/help.R` (markdown read it as an
  unfinished link) becomes plain `randomForestSRC`; the `95\%` escape in
  `R/gg_rfsrc.R::bootstrap_survival` becomes a literal `95%`. No API or
  rendered-doc behavioural change beyond the conventions switch.
- New
  [`gg_isopro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_isopro.md)
  and
  [`plot.gg_isopro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_isopro.md):
  tidy wrapper and ranked-elbow + density visualisation for
  [`varPro::isopro`](https://www.randomforestsrc.org/reference/isopro.html)
  isolation-forest anomaly scores.
  [`plot.gg_isopro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_isopro.md)
  takes `panel = c("both", "elbow", "density")` and optional `threshold`
  (score-space) or `top_n_pct` (quantile-space) to draw a reference
  line; if both are set, `threshold` wins with a message. A `method`
  column auto-triggers colour grouping for multi-method comparisons (use
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
  on three
  [`gg_isopro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_isopro.md)
  calls). `print` / `summary` / `autoplot` S3 companions follow the
  existing `gg_*` conventions. First of three Phase 4 sub-projects.
- [`plot.gg_variable()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_variable.md):
  fix render error on the default multi-class classification plot. The
  default-xvar selection was treating `yvar` (the observed-class column)
  and `outcome` (the multi-class pivot facet) as predictors; pivoting
  them into `var` then dropped the column the downstream
  `geom_jitter(aes(color = yvar))` referenced, and the patchwork errored
  when actually rendered. CI did not catch this because the existing
  test only asserted the patchwork class (lazy) and snapshots run with
  `VDIFFR_RUN_TESTS = false`. New test exercises a real build of every
  sub-plot.
- [`plot.gg_variable()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_variable.md):
  the same default-xvar selection used substring `grep("time", ...)` /
  `grep("event", ...)`, which silently dropped any predictor whose name
  contained those substrings – e.g. the documented veteran-data survival
  predictor `diagtime`. Switch to exact matching for `event` / `time` /
  `yvar` / `outcome` and an anchored prefix for `yhat` (`yhat` or
  `yhat.<class>`). New test exercises `diagtime` on the veteran survival
  forest.
- [`gg_roc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md):
  per-class one-vs-rest ROC curves
  ([\#88](https://github.com/ehrlinger/ggRandomForests/issues/88),
  closes
  [\#72](https://github.com/ehrlinger/ggRandomForests/issues/72)).
  - New `per_class` argument, default `FALSE`. With `per_class = TRUE`
    on a forest of more than two classes,
    [`gg_roc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
    returns a long-format `gg_roc` data frame with a `class` factor
    column, plus a named AUC vector attribute with one entry per class,
    ordered by descending AUC.
  - [`plot.gg_roc()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_roc.md)
    gains `panel = c("overlay", "facet")`. When the object has a `class`
    column, `"overlay"` colours the curves by class and `"facet"` gives
    each class its own panel.
  - [`summary.gg_roc()`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md)
    prints the named per-class AUC values when a `class` column is
    present.
  - On a binary forest, `per_class = TRUE` does nothing, the usual
    single-curve result comes back unchanged.
  - ROC confidence intervals are still to come, in v2.9.0 (issue
    [\#7](https://github.com/ehrlinger/ggRandomForests/issues/7) /
    [\#72](https://github.com/ehrlinger/ggRandomForests/issues/72)-CIs).
- New
  [`gg_udependent()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_udependent.md):
  varPro cross-variable dependency (Phase 3).
  - [`gg_udependent()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_udependent.md)
    reads cross-variable dependency scores off a `uvarpro` fit, via
    [`varPro::get.beta.entropy()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
    and
    [`varPro::sdependent()`](https://www.randomforestsrc.org/reference/utilities_internal.html).
    It returns a tidy list: `$edges` (variable_from, variable_to,
    weight), `$nodes` (variable, degree, selected), and `$graph`, an
    igraph object.
  - [`plot.gg_udependent()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_udependent.md)
    draws the dependency network with ggraph. Edge width and opacity
    scale with dependency strength; node colour marks the signal
    variables. The layout is configurable (`"fr"`, `"kk"`, `"stress"`,
    and so on).
  - `ggraph` added to `Suggests:`.
- New
  [`gg_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md):
  varPro variable importance
  ([\#85](https://github.com/ehrlinger/ggRandomForests/issues/85)).
  - [`gg_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md)
    pulls per-tree importance scores from a fitted `varpro` object and
    draws a boxplot of the per-tree z-score distribution for each
    variable. The hinges sit at the 15th and 85th percentiles and the
    whiskers at the 5th and 95th, so the box is not the usual Tukey one
    — it reports the percentiles it actually shows. Variables with
    aggregate z above `cutoff` (default 0.79) are colour-highlighted.
  - With `faithful = TRUE`, the individual per-tree z-scores are
    jittered over the box as semi-transparent points, with a
    white-outlined dot at the mean, the same view as varPro’s internal
    `bxp` output.
  - With `conditional = TRUE` (classification forests only),
    [`gg_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md)
    reads `$conditional.z` and draws class-conditional importance as a
    `facet_wrap(~class, nrow=1)` bar chart.
  - Set `local.std = FALSE` to allow `plot(..., type = "raw")`, which
    shows raw per-tree importance instead of the z-normalised values.

## ggRandomForests v2.8.0 (development)

- `gg_variable.randomForest`: classification fix
  ([\#87](https://github.com/ehrlinger/ggRandomForests/issues/87)).
  - For a classification forest,
    [`gg_variable.randomForest()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_variable.md)
    now stores per-class OOB vote fractions as `yhat.<classname>`
    columns, read from `object$votes`, the same layout the `rfsrc` path
    produces. It used to store a single `yhat` factor column of class
    labels (from `object$predicted`), and that column shape stopped the
    multi-class pivot in `plot.gg_variable` from ever running. The vote
    fractions are row-normalised to `[0, 1]`, even when the forest was
    fit with `norm.votes = FALSE`.
  - `plot.gg_variable`, binary classification: with `smooth = TRUE` the
    x and y aesthetics are now mapped onto the smooth layer correctly.
  - `plot.gg_variable`, multi-class numeric path: `smooth = TRUE` now
    adds the smooth layer instead of skipping it silently.
  - Closes stale issues
    [\#81](https://github.com/ehrlinger/ggRandomForests/issues/81)
    (fixed in PR
    [\#83](https://github.com/ehrlinger/ggRandomForests/issues/83)) and
    [\#82](https://github.com/ehrlinger/ggRandomForests/issues/82).
- New
  [`gg_partial_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md):
  varPro partial dependence
  ([\#84](https://github.com/ehrlinger/ggRandomForests/issues/84)).
  - [`gg_partial_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)
    takes over from
    [`gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)
    as the entry point for varPro partial dependence plots. It accepts
    an optional `object` argument (the originating `varpro` fit) which
    it uses for provenance-aware axis labels, and a `scale` argument
    (`"auto"`, `"mortality"`, `"rmst"`, `"surv"`, `"chf"`).
  - Ensemble mortality labelling (Ishwaran et al. 2008): with
    `scale = "mortality"`, or `scale = "auto"` on a survival forest, the
    y-axis reads “Ensemble mortality (expected events)”. That is an
    unbounded relative-risk score, not a survival probability, and the
    documentation says so plainly so it is not misread.
  - Survival path C: with `scale = "surv"` or `scale = "chf"`,
    [`gg_partial_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)
    pulls the embedded rfsrc forest from `object$rf` and returns true
    S(t) or CHF partial curves through the existing `gg_partial_rfsrc`
    machinery.
  - `varPro` is now a hard dependency (`Imports:`).
  - [`gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)
    is soft-deprecated: it warns, then hands off to
    [`gg_partial_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md).
    It will be removed in the release after v2.8.0.
- randomForest engine validation and repair
  ([\#82](https://github.com/ehrlinger/ggRandomForests/issues/82)).
  Fixes [\#80](https://github.com/ehrlinger/ggRandomForests/issues/80),
  [\#81](https://github.com/ehrlinger/ggRandomForests/issues/81), and a
  `plot.gg_error` label wart, and adds full randomForest regression test
  coverage. Details below.
  - [`plot.gg_variable()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_variable.md)
    now always returns a single `ggplot` (one variable) or a `patchwork`
    composite (several variables, or the default) — never a bare list.
    This matches the v2.7.3 `plot.gg_partial*` change. A list used to
    come back for multiple `xvar`, which broke `patchwork` /
    [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
    /
    [`layer_data()`](https://ggplot2.tidyverse.org/reference/ggplot_build.html)
    composition
    ([\#80](https://github.com/ehrlinger/ggRandomForests/issues/80)).
  - [`gg_roc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
    and
    [`calc_roc()`](https://ehrlinger.github.io/ggRandomForests/reference/calc_roc.rfsrc.md)
    for `randomForest` now build the ROC from class probabilities (OOB
    votes by default, honouring `oob`) rather than the degenerate
    three-point curve they produced before. With `which_outcome = "all"`
    (the default for `gg_roc(rf)`) the result is a macro-averaged
    one-vs-rest ROC, and no warning. The shared
    `.validate_which_outcome` helper and `calc_roc.rfsrc` are
    byte-for-byte unchanged, so rfsrc behaviour is untouched
    ([\#81](https://github.com/ehrlinger/ggRandomForests/issues/81)).
- Dependency modernization. This breaks scripts that relied on
  attachment. `randomForestSRC` and `randomForest` move from `Depends:`
  to `Imports:`; `igraph`, `callr`, and `varPro` are added to
  `Suggests:` (`varPro` later moves up to `Imports:`, with the first
  varPro-integration component).
  [`library(ggRandomForests)`](https://github.com/ehrlinger/ggRandomForests)
  no longer puts `randomForestSRC` or `randomForest` on the search path.
  A script that called
  [`rfsrc()`](https://www.randomforestsrc.org//reference/rfsrc.html) or
  [`randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  unqualified after only
  [`library(ggRandomForests)`](https://github.com/ehrlinger/ggRandomForests)
  now needs its own
  [`library(randomForestSRC)`](https://www.randomforestsrc.org/) /
  [`library(randomForest)`](https://www.stat.berkeley.edu/~breiman/RandomForests/),
  or must qualify the calls. ggRandomForests itself is unaffected. It
  qualifies every call into its dependencies.

## ggRandomForests v2.7.3

CRAN release: 2026-05-12

- [`plot.gg_partial()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial.md),
  [`plot.gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_rfsrc.md),
  and
  [`plot.gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_varpro.md)
  now always return a single `ggplot`/`patchwork` object. Previously,
  when both continuous and categorical predictors were present, they
  returned a named list `list(continuous=, categorical=)`, which
  surprised users and made
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  dispatch ambiguous. The two panels are now combined vertically via
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
  (patchwork moved from `Suggests` to `Imports`). Closes
  [\#77](https://github.com/ehrlinger/ggRandomForests/issues/77).
- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  S3 methods for all 10 `gg_*` classes, delegating to the corresponding
  `plot.gg_*()` method so objects work in `|>` pipelines, `patchwork`,
  and `cowplot` compositions via
  [`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
- [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) S3 methods for
  every `gg_*` data object (gg_error, gg_vimp, gg_rfsrc, gg_variable,
  gg_partial, gg_partial_rfsrc, gg_partialpro, gg_roc, gg_survival,
  gg_brier). [`print()`](https://rdrr.io/r/base/print.html) is
  header-only — use [`head()`](https://rdrr.io/r/utils/head.html) for
  rows. [`summary()`](https://rdrr.io/r/base/summary.html) returns a
  printable `summary.gg` object with per-class diagnostics. Each `gg_*`
  constructor now attaches a `"provenance"` attribute (source, family,
  ntree, n, xvar.names) consumed by the new methods.
- New
  [`gg_brier()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_brier.md)
  extractor and
  [`plot.gg_brier()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_brier.md)
  method for time-resolved Brier scores and CRPS on survival forests
  (issue [\#9](https://github.com/ehrlinger/ggRandomForests/issues/9)).
  Wraps
  [`randomForestSRC::get.brier.survival()`](https://www.randomforestsrc.org//reference/plot.survival.rfsrc.html)
  and adds the mortality-quartile decomposition, a 15-85 percent
  per-subject envelope, and running CRPS via trapezoidal integration.
  Supports `cens.model = c("km", "rfsrc")`, `type = c("brier", "crps")`,
  and `envelope` (overall line + 15-85% ribbon). Multi-model comparison
  is left to
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
  on multiple `gg_brier` outputs — see
  [`?gg_brier`](https://ehrlinger.github.io/ggRandomForests/reference/gg_brier.md)
  for an example.
- Visual unification of ribbon overlays across plot methods. All ribbons
  now use a shared alpha (`.gg_ribbon_alpha = 0.2`) and a shared fill
  (`.gg_ribbon_fill = "steelblue"`) for single-series cases (KM/NA CIs,
  bootstrap CIs, `gg_brier` envelope); group-stratified ribbons keep
  their group-coloured fill. Statistical bounds unchanged — only
  styling. ggRandomForests v2.7.2 =====================
- Address CRAN reviewer (Benjamin Altmann) feedback on the v2.7.1
  resubmission:
  - Add methods references to `DESCRIPTION` (Breiman 2001 and Ishwaran
    et al. 2008, with `<doi:...>` auto-links) per CRAN cookbook.
  - Drop the `man/shift.Rd` Rd file: `shift()` is an internal utility
    and the example used `ggRandomForests:::shift(...)`. Marked the
    function `@noRd` so it no longer generates a help page.
  - Replace [`cat()`](https://rdrr.io/r/base/cat.html) in
    [`surv_partial.rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/surv_partial.rfsrc.md)
    with [`message()`](https://rdrr.io/r/base/message.html) so progress
    output is suppressible
    ([`suppressMessages()`](https://rdrr.io/r/base/message.html)) and
    plays nicely inside notebooks / Shiny / quarto.
  - Restore the user’s [`par()`](https://rdrr.io/r/graphics/par.html)
    settings in the
    [`surv_partial.rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/surv_partial.rfsrc.md)
    example via
    `oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar))`.

## ggRandomForests v2.7.1

- Fix
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
  for survival forests:
  [`partial.rfsrc()`](https://www.randomforestsrc.org//reference/partial.rfsrc.html)
  was being called without `partial.type`, causing a zero-length
  comparison (`if (partial.type == "rel.freq") ...`) inside the C-level
  prediction routine and aborting the call. Survival forests now pass
  `partial.type = "surv"` (default; configurable via the new
  `partial.type` argument accepting `"surv"`, `"chf"`, or `"mort"`).
  This unblocks the `partial-dep` chunk in the survival vignette.
- Fix
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
  for survival forests with multiple `partial.time` values:
  [`get.partial.plot.data()`](https://www.randomforestsrc.org//reference/partial.rfsrc.html)
  returns yhat as an `[length(partial.values) x length(partial.time)]`
  matrix, but the previous code assumed a vector and crashed on
  column-mismatch when assigning `time`. The result is now reshaped to
  long form so each `(x, time)` pair is a single row.
- Improve
  [`plot.gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_rfsrc.md)
  survival layout: predictor value is now on the x-axis with one curve
  per (rounded) time point coloured by `Time`, faceted by variable name.
  The previous default put time on the x-axis and one curve per
  predictor value, producing a saturated legend with dozens of
  nearly-identical lines.
- Add `tests/testthat/test_plot_layer_data.R`: regression suite that
  uses
  [`ggplot2::layer_data()`](https://ggplot2.tidyverse.org/reference/ggplot_build.html)
  to verify each `plot.gg_*()` method renders non-empty layers for every
  supported forest family. Catches the empty-figure class of bug
  (transform/plot column-name mismatch) without requiring visual
  inspection.
- [`ggrandomforests.news()`](https://ehrlinger.github.io/ggRandomForests/reference/ggrandomforests.news.md)
  now reads `NEWS.md` (the canonical change log R also surfaces via
  [`utils::news()`](https://rdrr.io/r/utils/news.html)). The legacy
  hand-maintained `inst/NEWS` has been removed — it had silently drifted
  to v2.4.0 (June 2025) across three releases, so users running the
  helper saw stale version info. One source of truth, no more drift
  window.
- Fix
  [`plot.gg_vimp()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_vimp.md)
  legend duplication: the bar geom mapped both `fill` and `color` to the
  `positive` column, but only the fill legend was titled “VIMP \> 0”,
  leaving a redundant second legend titled “positive”. Both aesthetics
  now share the “VIMP \> 0” title so ggplot merges them into a single
  legend by default.
- Fix
  [`plot.gg_vimp()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_vimp.md)
  for forests with all-positive VIMP: the bar geom previously mapped
  only `color` (no `fill`), producing hollow / outline- only bars and an
  “Ignoring unknown labels: fill” warning whenever `labs(fill = ...)`
  was applied. Both `fill` and `color` are now mapped unconditionally,
  so bars render filled in every case.
- Add `@examples` blocks to
  [`plot.gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_rfsrc.md)
  and
  [`plot.gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_varpro.md).
  The latter uses a self-contained mock of the `varpro::partialpro()`
  output structure so the example runs without pulling in `varpro` as a
  dependency.

## ggRandomForests v2.7.0

- S3 design overhaul:
  [`gg_partial()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial.md),
  [`gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md),
  and
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
  now stamp their return values with S3 classes (`gg_partial`,
  `gg_partialpro`, `gg_partial_rfsrc` respectively), enabling
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) dispatch
  without any boilerplate.
- Add
  [`plot.gg_partial()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial.md),
  [`plot.gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_rfsrc.md),
  and
  [`plot.gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_varpro.md)
  S3 methods; continuous predictors render as line plots, categorical as
  bar charts, faceted by variable name. Survival forests produce curves
  over time; two-variable surface plots group by `xvar2.name`.
- Convert
  [`gg_survival()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_survival.md)
  to an S3 generic dispatching on the class of its first argument. New
  [`gg_survival.rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_survival.md)
  method extracts the survival response directly from the fitted forest
  (no separate data argument needed);
  [`gg_survival.default()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_survival.md)
  preserves the existing interface.
- Fix
  [`plot.gg_survival()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_survival.md)
  auto-coercion: previously called `gg_survival(rfsrc_obj)` treating the
  forest as the `interval` string argument, causing a latent crash;
  replaced with [`inherits()`](https://rdrr.io/r/base/class.html) guard.
- Deprecate
  [`surv_partial.rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/surv_partial.rfsrc.md)
  via [`.Deprecated()`](https://rdrr.io/r/base/Deprecated.html) with a
  pointer to
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md);
  all package tests updated to suppress the warning.
- Fix
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
  — `make_eval_grid()` used `unlist(dplyr::select())` which coerced
  factor columns to integer codes; now uses `newx[[xname]]` to preserve
  column class. Categorical detection extended to cover
  [`is.factor()`](https://rdrr.io/r/base/factor.html) and
  [`is.character()`](https://rdrr.io/r/base/character.html) in addition
  to the cardinality check.
- Add guards to
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md):
  all-NA `xval` after NA removal now emits a warning and skips the
  variable; all-NA grouping variable (`xvar2`) calls
  [`stop()`](https://rdrr.io/r/base/stop.html); `n_eval` and `cat_limit`
  are validated as single integers \>= 2 near function entry.
- Fix cyclomatic complexity across `gg_partial_rfsrc.R`: refactored into
  eight top-level unexported helpers (`validate_scalar_int`,
  `validate_partial_args`, `snap_partial_time`, `make_eval_grid`,
  `call_partial_rfsrc`, `partial_one_var`, `partial_no_group`,
  `partial_with_group`, `split_partial_result`); all functions now score
  below the `cyclocomp_linter` limit of 20.
- Fix `@param partial.time` documentation: “see the section above”
  corrected to “see the section below”.
- Replace deprecated
  [`tidyr::gather()`](https://tidyr.tidyverse.org/reference/gather.html)
  with
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
  in
  [`plot.gg_vimp()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_vimp.md)
  and
  [`plot.gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_varpro.md).
- Add `gg_survival.rfsrc`, `gg_survival.default`, `plot.gg_partial`,
  `plot.gg_partial_rfsrc`, and `plot.gg_partialpro` to `NAMESPACE`; add
  corresponding `@rdname` / `@export` roxygen tags.
- Update tests: add `expect_s3_class()` checks for all new classes; add
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) smoke tests
  for `gg_partial`, `gg_partial_rfsrc`, `gg_partialpro`; add
  `gg_survival.rfsrc` tests for KM extraction, `by` stratification, and
  error on non-survival forest.
- Add `plot.gg_partial`, `plot.gg_partial_rfsrc`, and
  `plot.gg_partialpro` to `_pkgdown.yml` reference index.

## ggRandomForests v2.7.0

- Fix critical visual bug in `plot.gg_rfsrc`: all
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) calls used
  bare string literals instead of `.data[[col]]`, causing every
  aesthetic to map to a constant string rather than the underlying data
  column. All plot types (regression, classification, survival) were
  affected.
- Fix [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html)
  bare-string literals in `plot.gg_roc` multi-class branch; remove
  unreachable `if (crv < 2)` dead-code branch.
- Fix `bootstrap_survival` CI-band indexing in `gg_rfsrc`: negative
  index computed via
  [`colnames()`](https://rdrr.io/r/base/colnames.html) was a no-op on
  large datasets and a latent crash for data with ≤ 2 unique event
  times.
- Fix `gg_rfsrc.rfsrc`: `is.null(df[, col])` does not detect missing
  columns; replaced with `!col %in% colnames()` guard.
- Fix `gg_rfsrc.randomForest`: method used non-existent `object$xvar`;
  now recovers the training frame via `.rf_recover_model_frame()`.
- Fix legend suppression in `plot.gg_error` for single-outcome forests
  where the data frame has no `variable` column.
- Fix `gg_vimp` and `plot.gg_vimp`: `1:nvar` replaced with
  `seq_len(nvar)` in both S3 methods; `1:0` silently returned `c(1, 0)`
  instead of `integer(0)` when `nvar == 0`.
- Migrate full test suite to testthat 3.x API: `expect_is` →
  `expect_s3_class` / `expect_type` / `expect_true(is.*())`;
  `expect_equivalent` → `expect_equal(ignore_attr = TRUE)`; all
  `context()` calls removed; testthat 1.x `expect_that` /
  `is_identical_to` removed.
- Add `.lintr` package-level linter configuration; fix lintr spacing in
  `gg_partial`.
- Improve GitHub Actions: `lint.yaml` now fails CI on any lint issue;
  `R-CMD-check.yaml` treats warnings as errors and uses Rtools 44;
  `test-coverage.yaml` duplicate codecov upload removed.
- Add `covr` and `vdiffr` to `Suggests`.

## ggRandomForests v2.6.1

- Fix model-label assignment in `gg_partial` for categorical variable
  data
- Refactor `gg_partial` and `gg_partial_rfsrc` to improve factor-level
  normalisation and categorical data handling

## ggRandomForests v2.6.0

- Add and export new plotting functions; update existing plot
  documentation
- Improve unit and integration tests; overall coverage raised to 83%
- Remove `hvtiRutilities` internal dependency; clean up associated
  imports
- Refactor `gg_partial_rfsrc` to use `.data` pronoun for all `dplyr`
  calls

## ggRandomForests v2.5.0

- Initial `gg_partial_rfsrc` function: computes partial dependence data
  directly from an `rfsrc` model via
  [`randomForestSRC::partial.rfsrc`](https://www.randomforestsrc.org//reference/partial.rfsrc.html),
  without requiring a separate `plot.variable` call
- Add support for a grouping variable (`xvar2.name`) in
  `gg_partial_rfsrc`
- Improved vignette formatting and namespace usage

## ggRandomForests v2.4.0

- Updating to latest ggplot2 functions
- Utilize some namespace referencing
- Added pkgdown documentation
- Minor testing improvements

## ggRandomForests v2.3.0

- Knocking the dust off this.
- Fix the ROC curves
- Fix the colors on VIMP plot

## ggRandomForests v2.2.1

CRAN release: 2022-09-01

- Fix docs for HTML5/Roxygen update

## ggRandomForests v2.2.0

CRAN release: 2022-05-09

- Bring back the regression vignette
- Improve package tests and code coverage
- Clean up code with lintr

## ggRandomForests v2.1.0

CRAN release: 2022-04-26

To pull this out of archive on randomForestSRC 3.1 build release. Fixed
a plot bug for gg_error to show the actual curve (issue 35)

## ggRandomForests v2.0.1

CRAN release: 2016-09-07

- Correct a bug in survival plots when predicting on future data without
  a known outcome.
- All Vignettes are now at <https://github.com/ehrlinger/ggRFVignette>
- All tests are being moved to
  <https://github.com/ehrlinger/ggRFVignette>
- Begin work on rewriting all checks to not use cached data. This will
  require more runtime, and hence we will run fewer of them on CRAN
  release.
- Minor bug and documentation fixes.

## ggRandomForests v2.0.0

CRAN release: 2016-06-11

- Added initial support for the randomForest package
- Updated cache files for randomForestSRC 2.2.0 release.
- Remove regression vignettes to meet CRAN size limits. These remain
  available at the package source
  <https://github.com/ehrlinger/ggRandomForests>
- Minor bug and documentation fixes.

## ggRandomForests v1.2.1

CRAN release: 2015-12-12

- Update cached datasets for randomForestSRC 2.0.0 release.
- Correct some vignette formatting errors (thanks Joe Smith)

## ggRandomForests v1.2.0

CRAN release: 2015-11-15

- Convert to semantic versioning <http://semver.org/>
- Updates for release of ggplot2 2.0.0
- Change from reshape2::melt dependence to tidyr::gather
- Optimize tests for CRAN to optimize R CMD CHECK times.

## ggRandomForests v1.1.4

CRAN release: 2015-03-29

- `combine.gg_partial` bug when giving a single variable plot.variable
  object.

- Remove `dplyr` depends to transitions from “Imports” to “Suggests”.

- Argument for single outcome `gg_vimp` plot for classification forests.

- Improvements to `gg_vimp` arguments for consistency.

- Add bootstrap confidence intervals to `gg_rfsrc` function.

- Initial `partial.rfsrc` function to replace the
  [`randomForestSRC::plot.variable`](https://www.randomforestsrc.org//reference/plot.variable.rfsrc.html)
  function.

- Move cache data to `randomForestSRC` v1.6.1 to take advantage of
  `rfsrc` version checking between function calls.

- Vignette updates for JSS submission of “ggRandomForests: Exploring
  Random Forest Survival”.

- Vignette updates for arXiv submission of ggRandomForests: Random
  Forests for Regression

- Some optimizations to reduce package size.

- Remove all tests from CRAN build to optimise R CMD CHECK times.

- Remove pdf vignette figure from CRAN build.

- Return S3method calls to NAMESPACE for “S3 methods exported but not
  registered” for R V3.2+.

- Misc Bug Fixes.

## ggRandomForests v1.1.3

CRAN release: 2015-01-08

- Update “ggRandomForests: Visually Exploring a Random Forest for
  Regression” vignette.
- Further development of draft package vignette “Survival with Random
  Forests”.
- Rename vignettes to align with randomForestSRC package usage.
- Add more tests and example functions.
- Refactor `gg_` functions into S3 methods to allow future
  implementation for other random forest packages.
- Improved help files.
- Updated DESCRIPTION file to remove redundant parts.
- Misc Bug Fixes.

## ggRandomForests v1.1.2

CRAN release: 2014-12-25

- Add package vignette “ggRandomForests: Visually Exploring a Random
  Forest for Regression”
- Add gg_partial_coplot, quantile_cuts and surface_matrix functions
- export the calc_roc and calc_auc functions.
- replace tidyr function dependency with reshape2 (melt instead of
  gather) due to lazy eval issues.
- reduce dplyr dependencies (remove select and %\>% usage for base
  equivalents, I still use tbl_df for printing)
- Further development of package vignette “Survival with Random Forests”
- Refactor cached example datasets for better documentation, estimates
  and examples.
- Improved help files.
- Updated DESCRIPTION file to remove redundant parts.
- Misc Bug Fixes.

## ggRandomForests v1.1.1

CRAN release: 2014-12-13

Maintenance release, mostly to fix gg_survival and gg_partial plots. \*
Fix the gg_survival functions to plot kaplan-meier estimates. \* Fix the
gg_partial functions for categorical variables. \* Add some more S3
print functions. \* Try to make gg_functions more consistent. \* Further
development of package vignette “Survival with Random Forests” \* Modify
the example cached datasets for better estimates and examples. \*
Improve help files. \* Misc Bug Fixes.

## ggRandomForests v1.1.0

CRAN release: 2014-12-05

- Add panel option for gg_variable and gg_partial
- Rework interactions plot
- add gg_coplot functions
- Imports instead of depends
- Add version dependencies for randomForestSRC
- Include package vignette “Random Forests for Survival”
- Misc Bug Fixes

## ggRandomForests v1.0.0

CRAN release: 2014-10-15

- First CRAN release.

## ggRandomForests v0.2

- Initial useR!2014 release.
