Package: ggRandomForests
Version: 3.1.1

ggRandomForests v3.1.1
======================
* CRAN fix: the varPro tests now call `skip_on_cran()` so they do not run
  on CRAN's check machines, including the gcc-UBSAN additional check. They
  were triggering an upstream `randomForestSRC` sanitizer issue (a 0-length
  array access in `rfsrcGrow`, `entry.c:184`) that surfaces when any
  `varPro` grow (`varpro()`, `beta.varpro()`, `uvarpro()`, `isopro()`,
  `ivarpro()`) builds a forest. ggRandomForests is pure R and its code is
  unchanged; the varPro tests run locally (`devtools::test()`) but are
  skipped under `R CMD check`, including our CI check jobs. The upstream
  issue has been reported to the randomForestSRC maintainers.
* The `varpro` vignette now loads every varPro fit from a precomputed
  file (`vignettes/varpro_precomputed.rds`, built by
  `vignettes/precompute_varpro.R`), so the vignette performs no live
  varPro grow during `R CMD check`. This removes the same upstream
  sanitizer path from the vignette build and trims check time. Each chunk
  falls back to a live fit if the precomputed object is absent, so the
  vignette remains reproducible from source.

ggRandomForests v3.1.0
======================
* Fix: `gg_vimp()` for single-outcome rfsrc forests now correctly flags
  variables with non-positive VIMP in the `positive` column (affecting
  plot coloring). The column was named `VIMP` (uppercase) in single-outcome
  fits but the flag check accessed `$vimp` (lowercase), leaving `positive`
  stuck at `TRUE` for all variables. Surfaced by the Copilot review on
  PR #109.
* Documentation pass. Deepened the varPro-family and rfsrc
  importance/partial/survival help pages against the upstream
  randomForestSRC and varPro documentation, and made the line between
  `gg_vimp()` (permutation, Breiman-Cutler importance) and `gg_varpro()`
  (varPro release-rule importance) explicit and cross-linked. Vignette
  prose deepened with the same framing; one-line code-comment fixes;
  fixed a stale `@return` in `gg_roc()` (documented a `yvar` column the
  function does not return). No user-facing behaviour change.
* Vignettes: the regression and survival partial-dependence surfaces are
  now rendered as static `ggplot2` heat maps instead of interactive
  `plotly` widgets, and figures render at 96 dpi. This cuts the installed
  size from ~17 MB to ~5 MB (the `plotly` library is no longer bundled into
  the vignette HTML). `plotly` is dropped from `Suggests`.
* Check time: reduced the `R CMD check` vignette-rebuild and test timings to
  bring the overall CRAN check comfortably under budget (CRAN flagged the
  overall check time on the 3.1.0 submission). The regression and survival
  vignettes use lighter forests (`ntree` 200 / 150, imputation `ntree` 100)
  and coarser partial-dependence grids. The varpro vignette's three
  `gg_partial_varpro()` calls and the Boston `beta.varpro()` fit (~34 s
  combined) are precomputed offline by `vignettes/precompute_varpro.R` and
  loaded from `vignettes/varpro_precomputed.rds`, with an automatic
  live-computation fallback if the file is absent. The `gg_udependent()`
  tests memoise the per-fit entropy matrix (`varPro::get.beta.entropy()`,
  ~1.5 s and a pure function of the fit) instead of recomputing it once per
  test. No user-facing behaviour change.

ggRandomForests v3.0.0
======================
* **Version jump to 3.0.0.** The varPro integration is a major scope
  expansion plus the `gg_partialpro()` soft-deprecation, which is
  major-version territory. Survival / multivariate varPro families,
  ROC confidence intervals, and hazard estimates are deferred to
  v3.1.0.
* CRAN-audit cleanup: the `gg_brier()` / `plot.gg_brier()` examples move
  from `\dontrun` to `\donttest` (so they execute under `R CMD check --as-cran` and on
  CRAN; `library(survival)` added so `Surv()` resolves), the
  per-variable `message()` in the deprecated `surv_partial.rfsrc()` is
  removed (its one behaviour change: that function no longer prints a
  line per variable), and the README points to the new "varpro"
  vignette.
* Fix: importance plots now consistently put the most-important variable
  at the **top**. `gg_varpro()`, `gg_beta_varpro()`, and `gg_ivarpro()`
  previously built their `variable` factor with descending levels, so
  after `coord_flip()` the most-important variable landed at the bottom
  — inverted relative to `gg_vimp()`. All three now reverse the factor
  levels to match the `gg_vimp` convention (and the `varImpPlot` / `vip`
  standard). Row order and `summary()` output are unchanged (still
  most-important first). A new cross-function test pins the convention.
* New vignette: "Exploring variable importance with varPro." Walks the
  full gg_* varPro layer (gg_partial_varpro, gg_varpro, gg_udependent,
  gg_isopro, gg_beta_varpro, gg_ivarpro) on three worked examples —
  regression (Boston), classification (iris binary + multi-class), and
  survival (PBC). Includes a family-support matrix documenting which
  wrapper works for which forest family. Headline document for v3.0.0.
* `gg_ivarpro()` and `plot.gg_ivarpro()`: tidy wrapper and
  per-variable-distribution / per-observation-profile plots for
  `varPro::ivarpro()` (individual / local variable importance) across
  regression and classification (binary + multi-class) families. The
  long-format tidy frame is `(obs, variable, local_imp, selected)` for
  regression; classification adds a `class` column. NA cells are
  filtered out and sparsity is surfaced in provenance. `which_obs`
  (integer index) collapses to a single-observation profile; the plot
  switches from a jittered distribution view to a horizontal bar
  chart. `which_class` (response level name) collapses to a single
  class panel; binary fits default to the last factor level (positive
  class). `cutoff` accepts `NULL` (per-class mean), a scalar, or a
  named numeric vector — matching the gg_beta_varpro classification
  contract. Optional `ivarpro_fit` argument lets callers cache the
  expensive `ivarpro()` call. Last of four Phase 4 sub-projects.
* `gg_beta_varpro()` adds varPro classification support (binary +
  multi-class). Binary fits default to a single positive-class panel
  (last factor level); multi-class fits return a long-format frame
  with a `class` column and plot as `facet_wrap(~ class)`. Optional
  `which_class` selects a single class; `cutoff` accepts a scalar or
  per-class named vector. Variables are stored as a factor whose
  levels are set by `mean(|sum-of-class-beta|)` descending so every
  facet shows rows in the same order. Motivating use case: 30-day
  mortality.
* Provenance shape change for `gg_beta_varpro()`:
  `attr(*, "provenance")$cutoff` is now always a named numeric
  vector — length 1 named `"regr"` for regression, length K named
  with the response factor levels for classification. Downstream
  tooling should read it as a vector and select by name; the prior
  scalar shape is gone.
* `gg_beta_varpro()` and `plot.gg_beta_varpro()`: tidy wrapper and default
  horizontal bar chart for `varPro::beta.varpro()` — the per-rule lasso-β
  refinement of variable importance. Aggregates per-rule β̂ by variable
  into `beta_mean = mean(|β̂|)` and flags variables above a selection
  cutoff (default `mean(beta_mean)`). Optional `beta_fit` argument lets
  callers compute the expensive `beta.varpro()` step once and reuse the
  result across multiple wrapper calls (different cutoffs, snapshot
  rebuilds, vignette knits). `print` / `summary` / `autoplot` S3
  companions follow the existing `gg_*` conventions. **Regression family
  only** — classification, regr+, and survival are tracked under Phase 4d
  (see the spec for the endpoint map). Third of three Phase 4 sub-projects.
* `gg_isopro()` gains a `newdata` argument so a fitted `varPro::isopro`
  model can score new observations into the same tidy `gg_isopro` frame.
  Internally the wrapper calls `predict.isopro()` twice: with
  `quantiles = FALSE` to populate the `case.depth` column (varPro's native
  polarity, lower = more anomalous) and with `quantiles = TRUE` to compute
  `howbad = 1 - quantile` (the wrapper convention, higher = more anomalous).
  Both polarities are visible in the returned data frame, and the
  relationship is named in the roxygen. The `plot` / `print` / `summary` /
  `autoplot` S3 companions work unchanged on the new tidy frame; to overlay
  training and test scores, bind the two extractor calls with a `method`
  label column and pass the result to `plot()`. Second of three Phase 4
  sub-projects.
* **Fix (gg_isopro training-path polarity).** Bug in the original
  `gg_isopro` (PR #94): varPro's `$howbad` on an `isopro` fit uses
  "lower = more anomalous" polarity (it is the quantile of `case.depth`),
  but the wrapper's plot method and documentation both assume "higher =
  more anomalous". Train scores and the new test-data scores were
  anti-correlated until this PR's training-path flip
  (`howbad = 1 - object$howbad`) brought them into agreement. The fix
  surfaced because the test-data sanity check (training-as-newdata top-5
  overlap) failed at 0/5 instead of 5/5 before the flip. Note: the two
  vdiffr baselines recorded in PR #94 (`gg-isopro-default` and
  `gg-isopro-threshold`) were recorded under the inverted polarity; they
  are visually flipped relative to the new behaviour but CI skips
  snapshots (`VDIFFR_RUN_TESTS = false`) so no failure surfaces. Re-record
  with `VDIFFR_RUN_TESTS = true` when convenient.
* Documentation: pedagogical pass over the varPro wrappers
  (`gg_partial_varpro`, `gg_varpro`, `gg_udependent` and their `plot.*`
  methods). Each help page now has explicit "What X is doing", "What's
  in the output", and "What you use this for" sections so a reader new
  to varPro can learn the underlying method (release rules, beta-entropy
  dependency, parametric / nonparametric / causal partial estimators)
  from the help page alone, not just the wrapper mechanics. No API or
  behavioural change.
* Documentation: enable roxygen2 markdown package-wide via
  `Roxygen: list(markdown = TRUE)` in `DESCRIPTION`. New roxygen blocks
  can use backticks and `[fn()]` link syntax; existing `\code{}` /
  `\link{}` markup keeps working. Two source-roxygen edits to keep
  R CMD check clean: `randomForest[SRC]` in `R/help.R` (markdown read
  it as an unfinished link) becomes plain `randomForestSRC`; the `95\%`
  escape in `R/gg_rfsrc.R::bootstrap_survival` becomes a literal `95%`.
  No API or rendered-doc behavioural change beyond the conventions
  switch.
* New `gg_isopro()` and `plot.gg_isopro()`: tidy wrapper and ranked-elbow +
  density visualisation for `varPro::isopro` isolation-forest anomaly
  scores. `plot.gg_isopro()` takes `panel = c("both", "elbow", "density")`
  and optional `threshold` (score-space) or `top_n_pct` (quantile-space)
  to draw a reference line; if both are set, `threshold` wins with a
  message. A `method` column auto-triggers colour grouping for multi-method
  comparisons (use `dplyr::bind_rows()` on three `gg_isopro()` calls).
  `print` / `summary` / `autoplot` S3 companions follow the existing `gg_*`
  conventions. First of three Phase 4 sub-projects.
* `plot.gg_variable()`: fix render error on the default multi-class
  classification plot. The default-xvar selection was treating `yvar` (the
  observed-class column) and `outcome` (the multi-class pivot facet) as
  predictors; pivoting them into `var` then dropped the column the
  downstream `geom_jitter(aes(color = yvar))` referenced, and the patchwork
  errored when actually rendered. CI did not catch this because the existing
  test only asserted the patchwork class (lazy) and snapshots run with
  `VDIFFR_RUN_TESTS = false`. New test exercises a real build of every
  sub-plot.
* `plot.gg_variable()`: the same default-xvar selection used substring
  `grep("time", ...)` / `grep("event", ...)`, which silently dropped any
  predictor whose name contained those substrings -- e.g. the documented
  veteran-data survival predictor `diagtime`. Switch to exact matching for
  `event` / `time` / `yvar` / `outcome` and an anchored prefix for `yhat`
  (`yhat` or `yhat.<class>`). New test exercises `diagtime` on the veteran
  survival forest.
* `gg_roc()`: per-class one-vs-rest ROC curves (#88, closes #72).
  - New `per_class` argument, default `FALSE`. With `per_class = TRUE` on a
    forest of more than two classes, `gg_roc()` returns a long-format
    `gg_roc` data frame with a `class` factor column, plus a named AUC
    vector attribute with one entry per class, ordered by descending AUC.
  - `plot.gg_roc()` gains `panel = c("overlay", "facet")`. When the object
    has a `class` column, `"overlay"` colours the curves by class and
    `"facet"` gives each class its own panel.
  - `summary.gg_roc()` prints the named per-class AUC values when a `class`
    column is present.
  - On a binary forest, `per_class = TRUE` does nothing, the usual
    single-curve result comes back unchanged.
  - ROC confidence intervals are still to come, in v3.1.0 (issue #7 / #72-CIs).
* New `gg_udependent()`: varPro cross-variable dependency (Phase 3).
  - `gg_udependent()` reads cross-variable dependency scores off a `uvarpro`
    fit, via `varPro::get.beta.entropy()` and `varPro::sdependent()`. It
    returns a tidy list: `$edges` (variable_from, variable_to, weight),
    `$nodes` (variable, degree, selected), and `$graph`, an igraph object.
  - `plot.gg_udependent()` draws the dependency network with ggraph. Edge
    width and opacity scale with dependency strength; node colour marks the
    signal variables. The layout is configurable (`"fr"`, `"kk"`,
    `"stress"`, and so on).
  - `ggraph` added to `Suggests:`.
* New `gg_varpro()`: varPro variable importance (#85).
  - `gg_varpro()` pulls per-tree importance scores from a fitted `varpro`
    object and draws a boxplot of the per-tree z-score distribution for each
    variable. The hinges sit at the 15th and 85th percentiles and the
    whiskers at the 5th and 95th, so the box is not the usual Tukey one —
    it reports the percentiles it actually shows. Variables with aggregate
    z above `cutoff` (default 0.79) are colour-highlighted.
  - With `faithful = TRUE`, the individual per-tree z-scores are jittered
    over the box as semi-transparent points, with a white-outlined dot at
    the mean, the same view as varPro's internal `bxp` output.
  - With `conditional = TRUE` (classification forests only), `gg_varpro()`
    reads `$conditional.z` and draws class-conditional importance as a
    `facet_wrap(~class, nrow=1)` bar chart.
  - Set `local.std = FALSE` to allow `plot(..., type = "raw")`, which shows
    raw per-tree importance instead of the z-normalised values.
* `gg_variable.randomForest`: classification fix (#87).
  - For a classification forest, `gg_variable.randomForest()` now stores
    per-class OOB vote fractions as `yhat.<classname>` columns, read from
    `object$votes`, the same layout the `rfsrc` path produces. It used to
    store a single `yhat` factor column of class labels (from
    `object$predicted`), and that column shape stopped the multi-class
    pivot in `plot.gg_variable` from ever running. The vote fractions are
    row-normalised to `[0, 1]`, even when the forest was fit with
    `norm.votes = FALSE`.
  - `plot.gg_variable`, binary classification: with `smooth = TRUE` the
    x and y aesthetics are now mapped onto the smooth layer correctly.
  - `plot.gg_variable`, multi-class numeric path: `smooth = TRUE` now adds
    the smooth layer instead of skipping it silently.
  - Closes stale issues #81 (fixed in PR #83) and #82.
* New `gg_partial_varpro()`: varPro partial dependence (#84).
  - `gg_partial_varpro()` takes over from `gg_partialpro()` as the entry
    point for varPro partial dependence plots. It accepts an optional
    `object` argument (the originating `varpro` fit) which it uses for
    provenance-aware axis labels, and a `scale` argument
    (`"auto"`, `"mortality"`, `"rmst"`, `"surv"`, `"chf"`).
  - Ensemble mortality labelling (Ishwaran et al. 2008): with
    `scale = "mortality"`, or `scale = "auto"` on a survival forest, the
    y-axis reads "Ensemble mortality (expected events)". That is an
    unbounded relative-risk score, not a survival probability, and the
    documentation says so plainly so it is not misread.
  - Survival path C: with `scale = "surv"` or `scale = "chf"`,
    `gg_partial_varpro()` pulls the embedded rfsrc forest from `object$rf`
    and returns true S(t) or CHF partial curves through the existing
    `gg_partial_rfsrc` machinery.
  - `varPro` is now a hard dependency (`Imports:`).
  - `gg_partialpro()` is soft-deprecated: it warns, then hands off to
    `gg_partial_varpro()`. It will be removed in the release after v3.0.0.
* randomForest engine validation and repair (#82). Fixes #80, #81, and a
  `plot.gg_error` label wart, and adds full randomForest regression test
  coverage. Details below.
  - `plot.gg_variable()` now always returns a single `ggplot` (one
    variable) or a `patchwork` composite (several variables, or the
    default) — never a bare list. This matches the v2.7.3
    `plot.gg_partial*` change. A list used to come back for multiple
    `xvar`, which broke `patchwork` / `autoplot()` / `layer_data()`
    composition (#80).
  - `gg_roc()` and `calc_roc()` for `randomForest` now build the ROC from
    class probabilities (OOB votes by default, honouring `oob`) rather
    than the degenerate three-point curve they produced before. With
    `which_outcome = "all"` (the default for `gg_roc(rf)`) the result is a
    macro-averaged one-vs-rest ROC, and no warning. The shared
    `.validate_which_outcome` helper and `calc_roc.rfsrc` are
    byte-for-byte unchanged, so rfsrc behaviour is untouched (#81).
* Dependency modernization. This breaks scripts that relied on attachment.
  `randomForestSRC` and `randomForest` move from `Depends:` to `Imports:`;
  `igraph`, `callr`, and `varPro` are added to `Suggests:` (`varPro` later
  moves up to `Imports:`, with the first varPro-integration component).
  `library(ggRandomForests)` no longer puts `randomForestSRC` or
  `randomForest` on the search path. A script that called `rfsrc()` or
  `randomForest()` unqualified after only `library(ggRandomForests)` now
  needs its own `library(randomForestSRC)` / `library(randomForest)`, or
  must qualify the calls. ggRandomForests itself is unaffected. It
  qualifies every call into its dependencies.

ggRandomForests v2.7.3
======================
* `plot.gg_partial()`, `plot.gg_partial_rfsrc()`, and `plot.gg_partialpro()`
  now always return a single `ggplot`/`patchwork` object. Previously, when
  both continuous and categorical predictors were present, they returned a
  named list `list(continuous=, categorical=)`, which surprised users and
  made `autoplot()` dispatch ambiguous. The two panels are now combined
  vertically via `patchwork::wrap_plots()` (patchwork moved from `Suggests`
  to `Imports`). Closes #77.
* `autoplot()` S3 methods for all 10 `gg_*` classes, delegating to the
  corresponding `plot.gg_*()` method so objects work in `|>` pipelines,
  `patchwork`, and `cowplot` compositions via `ggplot2::autoplot()`.
* `print()` and `summary()` S3 methods for every `gg_*` data object
  (gg_error, gg_vimp, gg_rfsrc, gg_variable, gg_partial,
  gg_partial_rfsrc, gg_partialpro, gg_roc, gg_survival, gg_brier).
  `print()` is header-only — use `head()` for rows. `summary()`
  returns a printable `summary.gg` object with per-class diagnostics.
  Each `gg_*` constructor now attaches a `"provenance"` attribute
  (source, family, ntree, n, xvar.names) consumed by the new methods.
* New `gg_brier()` extractor and `plot.gg_brier()` method for time-resolved
  Brier scores and CRPS on survival forests (issue #9). Wraps
  `randomForestSRC::get.brier.survival()` and adds the mortality-quartile
  decomposition, a 15-85 percent per-subject envelope, and running CRPS
  via trapezoidal integration. Supports `cens.model = c("km", "rfsrc")`,
  `type = c("brier", "crps")`, and `envelope` (overall line + 15-85%
  ribbon). Multi-model comparison is left to `dplyr::bind_rows()` on
  multiple `gg_brier` outputs — see `?gg_brier` for an example.
* Visual unification of ribbon overlays across plot methods. All
  ribbons now use a shared alpha (`.gg_ribbon_alpha = 0.2`) and a
  shared fill (`.gg_ribbon_fill = "steelblue"`) for single-series
  cases (KM/NA CIs, bootstrap CIs, `gg_brier` envelope); group-stratified
  ribbons keep their group-coloured fill. Statistical bounds unchanged —
  only styling.
ggRandomForests v2.7.2
=====================
* Address CRAN reviewer (Benjamin Altmann) feedback on the v2.7.1
  resubmission:
  - Add methods references to `DESCRIPTION` (Breiman 2001 and
    Ishwaran et al. 2008, with `<doi:...>` auto-links) per CRAN
    cookbook.
  - Drop the `man/shift.Rd` Rd file: `shift()` is an internal utility
    and the example used `ggRandomForests:::shift(...)`. Marked the
    function `@noRd` so it no longer generates a help page.
  - Replace `cat()` in `surv_partial.rfsrc()` with `message()` so
    progress output is suppressible (`suppressMessages()`) and plays
    nicely inside notebooks / Shiny / quarto.
  - Restore the user's `par()` settings in the
    `surv_partial.rfsrc()` example via
    `oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar))`.

ggRandomForests v2.7.1
=====================
* Fix `gg_partial_rfsrc()` for survival forests: `partial.rfsrc()` was being
  called without `partial.type`, causing a zero-length comparison
  (`if (partial.type == "rel.freq") ...`) inside the C-level prediction
  routine and aborting the call. Survival forests now pass
  `partial.type = "surv"` (default; configurable via the new `partial.type`
  argument accepting `"surv"`, `"chf"`, or `"mort"`). This unblocks the
  `partial-dep` chunk in the survival vignette.
* Fix `gg_partial_rfsrc()` for survival forests with multiple
  `partial.time` values: `get.partial.plot.data()` returns yhat as an
  `[length(partial.values) x length(partial.time)]` matrix, but the previous
  code assumed a vector and crashed on column-mismatch when assigning
  `time`. The result is now reshaped to long form so each `(x, time)` pair
  is a single row.
* Improve `plot.gg_partial_rfsrc()` survival layout: predictor value is now
  on the x-axis with one curve per (rounded) time point coloured by `Time`,
  faceted by variable name. The previous default put time on the x-axis
  and one curve per predictor value, producing a saturated legend with
  dozens of nearly-identical lines.
* Add `tests/testthat/test_plot_layer_data.R`: regression suite that uses
  `ggplot2::layer_data()` to verify each `plot.gg_*()` method renders
  non-empty layers for every supported forest family. Catches the
  empty-figure class of bug (transform/plot column-name mismatch) without
  requiring visual inspection.
* `ggrandomforests.news()` now reads `NEWS.md` (the canonical change log
  R also surfaces via `utils::news()`). The legacy hand-maintained
  `inst/NEWS` has been removed — it had silently drifted to v2.4.0
  (June 2025) across three releases, so users running the helper saw
  stale version info. One source of truth, no more drift window.
* Fix `plot.gg_vimp()` legend duplication: the bar geom mapped both
  `fill` and `color` to the `positive` column, but only the fill legend
  was titled "VIMP > 0", leaving a redundant second legend titled
  "positive". Both aesthetics now share the "VIMP > 0" title so ggplot
  merges them into a single legend by default.
* Fix `plot.gg_vimp()` for forests with all-positive VIMP: the bar geom
  previously mapped only `color` (no `fill`), producing hollow / outline-
  only bars and an "Ignoring unknown labels: fill" warning whenever
  `labs(fill = ...)` was applied. Both `fill` and `color` are now mapped
  unconditionally, so bars render filled in every case.
* Add `@examples` blocks to `plot.gg_partial_rfsrc()` and
  `plot.gg_partialpro()`. The latter uses a self-contained mock of the
  `varpro::partialpro()` output structure so the example runs without
  pulling in `varpro` as a dependency.

ggRandomForests v2.7.0
=====================
* S3 design overhaul: `gg_partial()`, `gg_partialpro()`, and
  `gg_partial_rfsrc()` now stamp their return values with S3 classes
  (`gg_partial`, `gg_partialpro`, `gg_partial_rfsrc` respectively), enabling
  `plot()` dispatch without any boilerplate.
* Add `plot.gg_partial()`, `plot.gg_partial_rfsrc()`, and
  `plot.gg_partialpro()` S3 methods; continuous predictors render as line
  plots, categorical as bar charts, faceted by variable name.  Survival
  forests produce curves over time; two-variable surface plots group by
  `xvar2.name`.
* Convert `gg_survival()` to an S3 generic dispatching on the class of its
  first argument.  New `gg_survival.rfsrc()` method extracts the survival
  response directly from the fitted forest (no separate data argument
  needed); `gg_survival.default()` preserves the existing interface.
* Fix `plot.gg_survival()` auto-coercion: previously called
  `gg_survival(rfsrc_obj)` treating the forest as the `interval` string
  argument, causing a latent crash; replaced with `inherits()` guard.
* Deprecate `surv_partial.rfsrc()` via `.Deprecated()` with a pointer to
  `gg_partial_rfsrc()`; all package tests updated to suppress the warning.
* Fix `gg_partial_rfsrc()` — `make_eval_grid()` used `unlist(dplyr::select())`
  which coerced factor columns to integer codes; now uses `newx[[xname]]` to
  preserve column class.  Categorical detection extended to cover
  `is.factor()` and `is.character()` in addition to the cardinality check.
* Add guards to `gg_partial_rfsrc()`: all-NA `xval` after NA removal now
  emits a warning and skips the variable; all-NA grouping variable (`xvar2`)
  calls `stop()`; `n_eval` and `cat_limit` are validated as single integers
  >= 2 near function entry.
* Fix cyclomatic complexity across `gg_partial_rfsrc.R`: refactored into
  eight top-level unexported helpers (`validate_scalar_int`,
  `validate_partial_args`, `snap_partial_time`, `make_eval_grid`,
  `call_partial_rfsrc`, `partial_one_var`, `partial_no_group`,
  `partial_with_group`, `split_partial_result`); all functions now score
  below the `cyclocomp_linter` limit of 20.
* Fix `@param partial.time` documentation: "see the section above" corrected
  to "see the section below".
* Replace deprecated `tidyr::gather()` with `tidyr::pivot_longer()` in
  `plot.gg_vimp()` and `plot.gg_partialpro()`.
* Add `gg_survival.rfsrc`, `gg_survival.default`, `plot.gg_partial`,
  `plot.gg_partial_rfsrc`, and `plot.gg_partialpro` to `NAMESPACE`; add
  corresponding `@rdname` / `@export` roxygen tags.
* Update tests: add `expect_s3_class()` checks for all new classes; add
  `plot()` smoke tests for `gg_partial`, `gg_partial_rfsrc`, `gg_partialpro`;
  add `gg_survival.rfsrc` tests for KM extraction, `by` stratification, and
  error on non-survival forest.
* Add `plot.gg_partial`, `plot.gg_partial_rfsrc`, and `plot.gg_partialpro`
  to `_pkgdown.yml` reference index.

ggRandomForests v2.7.0
=====================
* Fix critical visual bug in `plot.gg_rfsrc`: all `aes()` calls used bare
  string literals instead of `.data[[col]]`, causing every aesthetic to map
  to a constant string rather than the underlying data column. All plot
  types (regression, classification, survival) were affected.
* Fix `aes()` bare-string literals in `plot.gg_roc` multi-class branch;
  remove unreachable `if (crv < 2)` dead-code branch.
* Fix `bootstrap_survival` CI-band indexing in `gg_rfsrc`: negative index
  computed via `colnames()` was a no-op on large datasets and a latent crash
  for data with ≤ 2 unique event times.
* Fix `gg_rfsrc.rfsrc`: `is.null(df[, col])` does not detect missing columns;
  replaced with `!col %in% colnames()` guard.
* Fix `gg_rfsrc.randomForest`: method used non-existent `object$xvar`; now
  recovers the training frame via `.rf_recover_model_frame()`.
* Fix legend suppression in `plot.gg_error` for single-outcome forests where
  the data frame has no `variable` column.
* Fix `gg_vimp` and `plot.gg_vimp`: `1:nvar` replaced with `seq_len(nvar)`
  in both S3 methods; `1:0` silently returned `c(1, 0)` instead of
  `integer(0)` when `nvar == 0`.
* Migrate full test suite to testthat 3.x API: `expect_is` →
  `expect_s3_class` / `expect_type` / `expect_true(is.*())`;
  `expect_equivalent` → `expect_equal(ignore_attr = TRUE)`; all `context()`
  calls removed; testthat 1.x `expect_that` / `is_identical_to` removed.
* Add `.lintr` package-level linter configuration; fix lintr spacing in
  `gg_partial`.
* Improve GitHub Actions: `lint.yaml` now fails CI on any lint issue;
  `R-CMD-check.yaml` treats warnings as errors and uses Rtools 44;
  `test-coverage.yaml` duplicate codecov upload removed.
* Add `covr` and `vdiffr` to `Suggests`.

ggRandomForests v2.6.1
=====================
* Fix model-label assignment in `gg_partial` for categorical variable data
* Refactor `gg_partial` and `gg_partial_rfsrc` to improve factor-level
  normalisation and categorical data handling

ggRandomForests v2.6.0
=====================
* Add and export new plotting functions; update existing plot documentation
* Improve unit and integration tests; overall coverage raised to 83%
* Remove `hvtiRutilities` internal dependency; clean up associated imports
* Refactor `gg_partial_rfsrc` to use `.data` pronoun for all `dplyr` calls

ggRandomForests v2.5.0
=====================
* Initial `gg_partial_rfsrc` function: computes partial dependence data
  directly from an `rfsrc` model via `randomForestSRC::partial.rfsrc`, without
  requiring a separate `plot.variable` call
* Add support for a grouping variable (`xvar2.name`) in `gg_partial_rfsrc`
* Improved vignette formatting and namespace usage

ggRandomForests v2.4.0
=====================
* Updating to latest ggplot2 functions
* Utilize some namespace referencing
* Added pkgdown documentation
* Minor testing improvements

ggRandomForests v2.3.0
=====================
* Knocking the dust off this.
* Fix the ROC curves
* Fix the colors on VIMP plot

ggRandomForests v2.2.1
=====================
* Fix docs for HTML5/Roxygen update

ggRandomForests v2.2.0
=====================
* Bring back the regression vignette
* Improve package tests and code coverage
* Clean up code with lintr

ggRandomForests v2.1.0
=====================
To pull this out of archive on randomForestSRC 3.1 build release.
Fixed a plot bug for gg_error to show the actual curve (issue 35)

ggRandomForests v2.0.1
======================
* Correct a bug in survival plots when predicting on future data without a known outcome.
* All Vignettes are now at https://github.com/ehrlinger/ggRFVignette
* All tests are being moved to https://github.com/ehrlinger/ggRFVignette
* Begin work on rewriting all checks to not use cached data. 
  This will require more runtime, and hence we will run fewer of them on CRAN release. 
* Minor bug and documentation fixes.

ggRandomForests v2.0.0
======================
* Added initial support for the randomForest package
* Updated cache files for randomForestSRC 2.2.0 release.
* Remove regression vignettes to meet CRAN size limits. These remain available at the package source https://github.com/ehrlinger/ggRandomForests
* Minor bug and documentation fixes.

ggRandomForests v1.2.1
======================
* Update cached datasets for randomForestSRC 2.0.0 release. 
* Correct some vignette formatting errors (thanks Joe Smith)

ggRandomForests v1.2.0
======================
* Convert to semantic versioning http://semver.org/
* Updates for release of ggplot2 2.0.0
* Change from reshape2::melt dependence to tidyr::gather
* Optimize tests for CRAN to optimize R CMD CHECK times.


ggRandomForests v1.1.4
======================
* `combine.gg_partial` bug when giving a single variable plot.variable object.
* Remove `dplyr` depends to transitions from "Imports" to "Suggests".
* Argument for single outcome `gg_vimp` plot for classification forests.
* Improvements to `gg_vimp` arguments for consistency.
* Add bootstrap confidence intervals to `gg_rfsrc` function.
* Initial `partial.rfsrc` function to replace the `randomForestSRC::plot.variable` function.
* Move cache data to `randomForestSRC` v1.6.1 to take advantage of `rfsrc` version checking between function calls.

* Vignette updates for JSS submission of "ggRandomForests: Exploring Random Forest Survival".
* Vignette updates for arXiv submission of ggRandomForests: Random Forests for Regression

* Some optimizations to reduce package size.
* Remove all tests from CRAN build to optimise R CMD CHECK times.
* Remove pdf vignette figure from CRAN build.
* Return S3method calls to NAMESPACE for "S3 methods exported but not registered" for R V3.2+.
  
* Misc Bug Fixes.

ggRandomForests v1.1.3
======================
* Update "ggRandomForests: Visually Exploring a Random Forest for Regression" vignette.
* Further development of draft package vignette "Survival with Random Forests". 
* Rename vignettes to align with randomForestSRC package usage.
* Add more tests and example functions.
* Refactor `gg_` functions into S3 methods to allow future implementation for other random forest packages.
* Improved help files.
* Updated DESCRIPTION file to remove redundant parts.
* Misc Bug Fixes.

ggRandomForests v1.1.2
======================
* Add package vignette "ggRandomForests: Visually Exploring a Random Forest for Regression"
* Add gg_partial_coplot, quantile_cuts and surface_matrix functions
* export the calc_roc and calc_auc functions.
* replace tidyr function dependency with reshape2 (melt instead of gather) due to lazy eval issues.
* reduce dplyr dependencies (remove select and %>% usage for base equivalents, I still use tbl_df for printing)
* Further development of package vignette "Survival with Random Forests" 
* Refactor cached example datasets for better documentation, estimates and examples.
* Improved help files.
* Updated DESCRIPTION file to remove redundant parts.
* Misc Bug Fixes.


ggRandomForests v1.1.1
======================
Maintenance release, mostly to fix gg_survival and gg_partial plots.
* Fix the gg_survival functions to plot kaplan-meier estimates.
* Fix the gg_partial functions for categorical variables.
* Add some more S3 print functions.
* Try to make gg_functions more consistent.
* Further development of package vignette "Survival with Random Forests" 
* Modify the example cached datasets for better estimates and examples.
* Improve help files.
* Misc Bug Fixes.


ggRandomForests v1.1.0
======================
* Add panel option for gg_variable and gg_partial
* Rework interactions plot
* add gg_coplot functions
* Imports instead of depends
* Add version dependencies for randomForestSRC
* Include package vignette "Random Forests for Survival" 
* Misc Bug Fixes

ggRandomForests v1.0.0
======================
* First CRAN release.

ggRandomForests v0.2
======================
* Initial useR!2014 release. 
