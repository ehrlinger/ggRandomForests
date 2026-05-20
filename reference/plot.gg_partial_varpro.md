# Plot a [`gg_partial_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md) object

Produces ggplot2 partial dependence curves from the named list returned
by
[`gg_partial_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md).
Continuous predictors are shown as overlaid line curves (one per effect
type); categorical predictors as side-by-side boxplots. For survival
path-C objects (produced when `scale %in% c("surv","chf")` is passed to
the extractor) the plot is delegated to
[`plot.gg_partial_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_rfsrc.md).

## Usage

``` r
# S3 method for class 'gg_partialpro'
plot(x, type = c("parametric", "nonparametric", "causal"), ...)

# S3 method for class 'gg_partial_varpro'
plot(x, type = c("parametric", "nonparametric", "causal"), ...)
```

## Arguments

- x:

  A
  [`gg_partial_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)
  object.

- type:

  Character vector; one or more of `"parametric"`, `"nonparametric"`,
  `"causal"`. Defaults to all three. Ignored for path-C objects.

- ...:

  Not currently used for path-A objects; forwarded to
  `plot.gg_partial_rfsrc` for path-C objects.

## Value

A `ggplot` (or `patchwork`) object.

## Details

\*\*Ensemble mortality (scale = "mortality"):\*\* When the provenance
scale is `"mortality"`, the y-axis label reads *"Ensemble mortality
(expected events)"* to make clear that this is an **unbounded
relative-risk score**, not a survival probability or \\1 - S(t)\\
(Ishwaran, Kogalur, Blackstone & Lauer, 2008
\<doi:10.1214/08-AOAS169\>).

## References

Ishwaran H, Kogalur UB, Blackstone EH, Lauer MS (2008). Random survival
forests. *The Annals of Applied Statistics*, **2**(3), 841–860.
[doi:10.1214/08-AOAS169](https://doi.org/10.1214/08-AOAS169) .

## See also

[`gg_partial_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)

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
pp <- gg_partial_varpro(mock_data)
plot(pp)

plot(pp, type = "parametric")

```
