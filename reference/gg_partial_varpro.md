# Partial dependence data from a varPro model

[`varPro::partialpro`](https://luminwin.github.io/reference/partialpro.html)
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
  scale = c("auto", "rmst", "mortality", "surv", "chf"),
  time = NULL,
  nvars = NULL,
  cat_limit = 10,
  model = NULL
)

gg_partialpro(
  part_dta,
  object = NULL,
  scale = c("auto", "rmst", "mortality", "surv", "chf"),
  time = NULL,
  nvars = NULL,
  cat_limit = 10,
  model = NULL
)
```

## Arguments

- part_dta:

  Passed to `gg_partial_varpro`.

- object:

  Passed to `gg_partial_varpro`.

- scale:

  Passed to `gg_partial_varpro`.

- time:

  Passed to `gg_partial_varpro`.

- nvars:

  Passed to `gg_partial_varpro`.

- cat_limit:

  Passed to `gg_partial_varpro`.

- model:

  Passed to `gg_partial_varpro`.

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

**Scale detection:** with `scale = "auto"` and an `object` in hand, the
scale resolves to `"mortality"` for a survival forest and `"generic"`
for a regression or classification forest. The RMST horizon \\\tau\\ is
*not* stored in the `varpro` object (varPro 3.1.0), so for RMST-labeled
output you have to pass `scale = "rmst", time = tau` yourself.

**Ensemble mortality (scale = "mortality"):** here the y-axis is
*ensemble mortality*, the expected number of events a subject would see
if they were exposed to the study-average cumulative hazard. It is the
same quantity as the `rfsrc` `predicted` value for survival forests
(Ishwaran, Kogalur, Blackstone & Lauer, 2008 <doi:10.1214/08-AOAS169>).
This is an **unbounded relative-risk score**, *not* a survival
probability and not \\1 - S(t)\\; don't read it as one. If you want
output on the probability scale, refit with `varpro(..., rmst = tau)`
and use `scale = "rmst"`.

## What partialpro is doing

A partial dependence curve answers the question, "if I hold a single
variable at a grid of values and average out everything else, how does
the model's prediction move?" That is the same question `rfsrc` partial
dependence answers. What
[`varPro::partialpro`](https://luminwin.github.io/reference/partialpro.html)
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
[`varPro::partialpro`](https://luminwin.github.io/reference/partialpro.html)):
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
```
