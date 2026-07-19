# Partial dependence data from a varPro model

[`varPro::partialpro`](https://www.randomforestsrc.org/reference/partialpro.html)
returns one list, with continuous and categorical predictors mixed
together. This function splits that list into two tidy data frames, one
for each kind, and resolves the y-axis label the plot method will use.

*Deprecated.* `gg_partialpro()` has been superseded by
`gg_partial_varpro()` and is now a thin alias for it. It will be removed
in the release after ggRandomForests v3.0.0; use `gg_partial_varpro()`
directly.

## Usage

``` r
gg_partial_varpro(
  part_dta = NULL,
  object = NULL,
  scale = c("auto", "prob", "odds", "logodds", "rmst", "surv", "mortality", "chf"),
  time = NULL,
  nvars = NULL,
  cat_limit = 10,
  model = NULL,
  ...
)

gg_partialpro(
  part_dta,
  object = NULL,
  scale = c("auto", "prob", "odds", "logodds", "rmst", "surv", "mortality", "chf"),
  time = NULL,
  nvars = NULL,
  cat_limit = 10,
  model = NULL,
  ...
)
```

## Arguments

- part_dta:

  Partial plot data from
  [`varPro::partialpro`](https://www.randomforestsrc.org/reference/partialpro.html).
  Each element must contain `xvirtual`, `xorg`, `yhat.par`,
  `yhat.nonpar`, and `yhat.causal`. Supply at least one of `part_dta` or
  `object`.

- object:

  A fitted `varpro` object, the forest the partial data came from. When
  supplied it provides the provenance metadata, and when `part_dta` is
  `NULL` it is passed to `varPro::partialpro(object)` for you. Required
  when `scale %in% c("surv","chf")`.

- scale:

  Character; the y-axis scale. One of `"auto"` (default), the
  classification scales `"prob"` / `"odds"` / `"logodds"`, or the
  survival scales `"rmst"` / `"surv"` / `"mortality"` / `"chf"`. With
  `"auto"`: classification fits resolve to `"prob"` (probability of the
  target class) and survival fits to `"surv"` (survival probability at a
  default horizon \\\tau\\); see **Details**.

- time:

  Numeric; the evaluation time point. Required when `scale = "rmst"`
  (the RMST horizon \\\tau\\), where it now *drives* the partial
  computation through an RMST(\\\tau\\) learner (see **Details**), not
  just the axis label. Optional when `scale %in% c("surv","chf")`: if
  supplied it is snapped to the nearest value in
  `object\$rf\$time.interest` and used for both computation and axis
  labeling; if `NULL`, three quartile time points from `time.interest`
  are used (see
  [`gg_partial_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)).

- nvars:

  Integer; how many variables (list elements) to process. Defaults to
  every variable in `part_dta`.

- cat_limit:

  Integer; a variable with `length(xvirtual) <= cat_limit` is treated as
  categorical. Default `10`.

- model:

  Character; a label tacked onto every row, handy when you are combining
  results from several models in one figure.

- ...:

  Forwarded to
  [`partialpro`](https://www.randomforestsrc.org/reference/partialpro.html)
  on the object-driven path (when `part_dta` is `NULL`). Use this to
  control which variables are computed – e.g. `xvar.names` or `nvar` –
  or to tune the isolation-forest UVT step (`cut`, `nsmp`). Without it,
  `partialpro` falls back to `varPro::get.topvars(object)`, which can
  return few or no variables for some fits (yielding empty
  `continuous`/`categorical` frames). A name you pass in `xvar.names`
  that the fit cannot reach is dropped by `partialpro` without comment,
  so you can ask for twelve variables and get ten; we warn and name the
  missing ones. See **Details**. Ignored, with a warning, when
  `part_dta` is supplied. With `scale = "chf"` the work goes through
  [`gg_partial_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
  rather than `partialpro`, so `xvar.names` is honored but the
  `partialpro`-only arguments (`cut`, `nsmp`) do not apply and are
  ignored with a warning.

## Value

A named list of class `"gg_partial_varpro"` with elements:

- continuous:

  data.frame with columns `variable`, `parametric`, `nonparametric`,
  `causal`, `name` (and optionally `model`).

- categorical:

  data.frame with the same columns, one row per observation per category
  level.

A `"provenance"` attribute carries `source`, `family`, `ntree`, `n`,
`scale`, `rmst_tau`, `xvar.names`, and `path`.

A `gg_partial_varpro` object (see `gg_partial_varpro`).

## Details

**Which variables you actually get:** `varpro` screens twice before
anything reaches partial dependence. The split-weight screen decides
which predictors are worth guiding the trees with, and what survives it
lands in `object$xvar.names`;
[`varPro::get.topvars`](https://www.randomforestsrc.org/reference/utilities_internal.html)
then ranks a shorter list out of that. So the design matrix, the
reachable set, and the default list are three different sizes – a fit on
45 predictors might carry 26 in `object$xvar.names` and 15 in
`get.topvars`. `partialpro` can only reach the middle one. How much gets
screened off depends on the data and the fit, so check rather than
assume: `length(object$xvar.names)` against `ncol(object$x)` tells you
where you stand.

This bites when you bring a variable list in from somewhere else, say
the top names off an `rfsrc` VIMP ranking. `partialpro` intersects your
`xvar.names` with what it can reach and keeps the overlap without
remarking on it, so a request for twelve variables can come back with
ten and nothing in the result says so. It is the intermittent kind of
trap: a top-10 list may come back whole while a top-12 list quietly
loses two. We compare the two sets before calling `partialpro` and warn,
naming what was dropped. A quick `setdiff(my_names, object$xvar.names)`
answers the same question before you spend the computation.

For a complete view, fit with both screens off:
`varPro::varpro(..., sparse = FALSE, split.weight = FALSE)`.
`split.weight = FALSE` is the one that lifts the ceiling – it puts every
predictor in `object$xvar.names`, so partial dependence can reach them
all, and it leaves a strong variable's curve where it was.
`sparse = FALSE` does the smaller thing, deepening
[`varPro::get.topvars`](https://www.randomforestsrc.org/reference/utilities_internal.html)
so the reported ranking shows its tail rather than the screened top.
Both are varPro's own arguments, and its defaults (both `TRUE`) go the
other way, toward the screened set; keep the defaults when that sparser
set is what you want. `nvar` is not the knob here – it only caps how
much gets reported.

**Scale detection:** with `scale = "auto"` and an `object` in hand, the
scale resolves to `"mortality"` for a survival forest and `"generic"`
for a regression or classification forest. The RMST horizon \\\tau\\ is
*not* stored in the `varpro` object (varPro 3.1.0), so RMST output
requires you to pass `scale = "rmst", time = tau` explicitly.

**RMST partial dependence (scale = "rmst"):**
[`varPro::partialpro`](https://www.randomforestsrc.org/reference/partialpro.html)
has no time argument, so its default survival learner returns ensemble
mortality at every horizon – passing a horizon through `...` is silently
dropped, and multi-horizon plots built that way differ only by
Monte-Carlo noise, not by \\\tau\\. To get a genuine RMST(\\\tau\\)
curve, `scale = "rmst"` supplies `partialpro` a `learner` that returns
\\\mathrm{RMST}(\tau)=\int_0^\tau S(t)\\dt\\ from the survival forest,
so the curve actually depends on \\\tau\\. This path **recomputes** from
`object`, so it needs `object` (a survival fit) with `part_dta = NULL`;
a precomputed `part_dta` can only be relabeled, and `gg_partial_varpro`
warns when you try. A \\\tau\\ beyond the model's largest event time is
truncated there (with a warning), since \\S(t)\\ cannot be extrapolated.

**Classification scale (scale = "prob"/"odds"/"logodds"):**
[`varPro::partialpro`](https://www.randomforestsrc.org/reference/partialpro.html)
returns classification effects as *log-odds* of the target class.
`scale = "prob"` (the classification default) back-transforms to
probability \\P(Y = \mathrm{target})\\, `"odds"` to the odds, and
`"logodds"` keeps the raw scale. The back-transform is applied per
observation *before* averaging, so the curve is the mean predicted
probability, not the probability of the mean log-odds. The `causal`
contrast is shown only on `"logodds"` (see
[`plot.gg_partial_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_varpro.md)).

**Survival probability (scale = "surv"):** `scale = "surv"` (the
survival default) computes \\S(\tau \mid x)\\ through `partialpro` (the
same UVT engine as mortality and RMST), bounded in \\\[0, 1\]\\. When
`time` is not supplied, \\\tau\\ defaults to the **median follow-up
time** of the fit – a data-driven horizon that is always in the model's
own time units, so it cannot be mis-specified the way a hand-typed
\\\tau\\ can. The resolved \\\tau\\ is reported in a message and the
axis label; pass `time = tau` to choose another. `scale = "mortality"`
keeps the unbounded ensemble-mortality score as an explicit opt-in.

**Ensemble mortality (scale = "mortality"):** here the y-axis is
*ensemble mortality*, the expected number of events a subject would see
if they were exposed to the study-average cumulative hazard. It is the
same quantity as the `rfsrc` `predicted` value for survival forests
(Ishwaran, Kogalur, Blackstone & Lauer, 2008 <doi:10.1214/08-AOAS169>).
This is an **unbounded relative-risk score**, *not* a survival
probability and not \\1 - S(t)\\; don't read it as one. For a bounded,
time-anchored survival summary, use `scale = "rmst", time = tau`
(restricted mean survival time, in the time units of the outcome) or
`scale = "surv"` / `"chf"`.

Arguments are documented on `gg_partial_varpro`; this alias shares its
formals and forwards every argument unchanged.

## What partialpro is doing

A partial dependence curve answers the question, "if I hold a single
variable at a grid of values and average out everything else, how does
the model's prediction move?" That is the same question `rfsrc` partial
dependence answers. What
[`varPro::partialpro`](https://www.randomforestsrc.org/reference/partialpro.html)
adds is two wrinkles that are worth understanding before you read the
curves.

First, `partialpro` filters the partial grid through an isolation forest
(Unlimited Virtual Twins, or UVT) so that unlikely combinations of the
focal variable with the rest of the data are downweighted. The `rfsrc`
version, by contrast, averages over the full marginal grid regardless of
plausibility. So when a covariate is highly correlated with others, the
two methods can disagree, and `partialpro`'s curve is the one restricted
to the data manifold.

Second, `partialpro` fits a local polynomial model to the predicted
values rather than just plotting their mean. That gives three parallel
curves per variable, stored as `yhat.par`, `yhat.nonpar`, and
`yhat.causal`, which the plot method overlays so you can see whether a
smooth parametric story and the raw forest predictions are telling you
the same thing.

Interpretation of the y-axis depends on the outcome (per
[`varPro::partialpro`](https://www.randomforestsrc.org/reference/partialpro.html)):
response scale for regression, log-odds of the target class for
classification, and either ensemble mortality (default) or RMST (if the
original `varpro` call set `rmst`) for survival.

## What's in the output

We split `partialpro`'s mixed list into two tidy data frames so the plot
method does not have to. A variable with more than `cat_limit` distinct
grid points goes into `$continuous`, one row per grid point with the
column means of `yhat.par`, `yhat.nonpar`, and `yhat.causal` stored as
`parametric`, `nonparametric`, and `causal`. A variable at or below
`cat_limit` goes into `$categorical`, one row per observation per
category level, carrying the same three columns unaveraged so the plot
method can draw boxplots. Path C (`scale %in% c("surv","chf")`) takes a
different route: we hand the underlying `rfsrc` forest to
`gg_partial_rfsrc` so you get a survival-probability or
cumulative-hazard curve on the usual rfsrc scale instead.

## What you use this for

- read the marginal shape of a relationship the varpro model found
  important: monotone, threshold, U-shape, flat;

- compare the three partialpro estimators on the same variable and flag
  the ones where parametric and nonparametric disagree, those are the
  candidates for closer inspection;

- report a survival partial dependence on the probability or
  cumulative-hazard scale (`scale = "surv"` or `"chf"`) rather than the
  unbounded mortality scale.

A varpro partial dependence curve is a description of the model, not a
causal effect. The `causal` column is varpro's local estimator, not a
structural causal claim about the data-generating process.

## References

Ishwaran H, Kogalur UB, Blackstone EH, Lauer MS (2008). Random survival
forests. *The Annals of Applied Statistics*, **2**(3), 841–860.
[doi:10.1214/08-AOAS169](https://doi.org/10.1214/08-AOAS169) .

Ishwaran H, Blackstone EH (2025). Harnessing the power of virtual
(digital) twins: Graphical causal tools for understanding patient and
hospital differences. *Computational and Structural Biotechnology
Journal*, **28**, 312.

## See also

[`plot.gg_partial_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_varpro.md),
[`gg_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md),
[`gg_vimp`](https://ehrlinger.github.io/ggRandomForests/reference/gg_vimp.md),
`gg_partialpro` (deprecated),
[`gg_partial_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md),
[`varpro_feature_names`](https://ehrlinger.github.io/ggRandomForests/reference/varpro_feature_names.md)

`gg_partial_varpro`

## Examples

``` r
set.seed(42)
n_obs <- 30; n_pts <- 15
mock_data <- list(
  age = list(
    xvirtual    = seq(30, 80, length.out = n_pts),
    xorg        = sample(seq(30, 80, by = 5), n_obs, replace = TRUE),
    yhat.par    = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
    yhat.nonpar = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
    yhat.causal = matrix(rnorm(n_obs * n_pts), nrow = n_obs)
  ),
  sex = list(
    xvirtual    = c(0, 1),
    xorg        = sample(c(0, 1), n_obs, replace = TRUE),
    yhat.par    = matrix(rnorm(n_obs * 2), nrow = n_obs),
    yhat.nonpar = matrix(rnorm(n_obs * 2), nrow = n_obs),
    yhat.causal = matrix(rnorm(n_obs * 2), nrow = n_obs)
  )
)
result <- gg_partial_varpro(mock_data)
head(result$continuous)
#> # A tibble: 6 × 5
#>   variable parametric nonparametric  causal name 
#>      <dbl>      <dbl>         <dbl>   <dbl> <chr>
#> 1     30      -0.0695       -0.120   0.436  age  
#> 2     33.6     0.149        -0.477   0.0438 age  
#> 3     37.1    -0.237        -0.270   0.0531 age  
#> 4     40.7    -0.0346       -0.0673 -0.0482 age  
#> 5     44.3    -0.136         0.171   0.176  age  
#> 6     47.9     0.0217        0.0159  0.119  age  
head(result$categorical)
#> # A tibble: 6 × 5
#>   parametric nonparametric causal variable name 
#>        <dbl>         <dbl>  <dbl>    <dbl> <chr>
#> 1      0.850       -0.0338  0.751        0 sex  
#> 2      1.76        -0.901  -0.829        0 sex  
#> 3      0.846       -1.18    0.710        0 sex  
#> 4     -0.545       -0.448   0.950        0 sex  
#> 5      0.255        0.559  -0.579        0 sex  
#> 6      0.299        0.0931 -1.23         0 sex  

# \donttest{
## The object-driven path: hand gg_partial_varpro() the varpro fit and let
## it call partialpro() for you.  This is where the reachability ceiling
## shows up (see Details).
set.seed(42)
vp <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)

## Three different sizes.  partialpro() can only reach the second.
ncol(vp$x)                    # predictors in the data
#> [1] 10
length(vp$xvar.names)         # what the fit reaches
#> [1] 7
length(varPro::get.topvars(vp))   # the default when xvar.names is absent
#> [1] 4

## Say these came from an rfsrc VIMP ranking.  Check what the fit cannot
## reach before you spend the computation -- this is the habit worth having.
wanted <- c("wt", "hp", "qsec", "vs")
setdiff(wanted, vp$xvar.names)
#> [1] "qsec" "vs"  

## Ask anyway and we warn, naming what partialpro() would have dropped
## in silence.
pd <- gg_partial_varpro(object = vp, xvar.names = wanted)
#> Warning: gg_partial_varpro: 2 of 4 requested 'xvar.names' are not in the varpro fit's reachable set and are silently dropped by varPro::partialpro(): qsec, vs. The fit reaches 7 of 10 predictors (object$xvar.names); varpro() screens in two stages, so a variable can be in the data and still be unreachable. Refit with varPro::varpro(..., split.weight = FALSE) to reach every predictor.

## Refitting without the split-weight screen reaches every predictor.
vp_all <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50,
                         split.weight = FALSE)
length(vp_all$xvar.names)
#> [1] 10
setdiff(wanted, vp_all$xvar.names)   # empty; nothing to drop
#> character(0)
# }
```
