# Plot a `gg_ivarpro` object

Branches on the presence of `which_obs` provenance and the `class`
column. Distribution view: jittered points showing per-observation local
importances per variable. Per-observation view: horizontal bar chart of
one observation's local importances across variables. Classification:
faceted by class unless `which_class` collapses to a single class.

## Usage

``` r
# S3 method for class 'gg_ivarpro'
plot(x, ...)
```

## Arguments

- x:

  A `gg_ivarpro` object from
  [`gg_ivarpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_ivarpro.md).

- ...:

  Not currently used.

## Value

A `ggplot` object.

## Reading the chart

Each point in the distribution view is one observation's local
importance for that variable. Variables are sorted by descending
`mean(|local_imp|)`. The cutoff line picks the variables whose local
importance is, on average, large enough to flag. For a classification
fit, every facet shares the same row order so you can read across.

For a classification fit, variables are sorted by descending
`mean(|local_imp|)` across all (obs, class) rows and that ordering is
shared across every facet, so rows line up between classes for visual
comparison. Each facet has its own cutoff line.

The per-observation view (`which_obs`) is a horizontal bar chart of one
observation's local importances; bars below the cutoff are gray, above
are blue. The visual resembles a SHAP waterfall, but the values are
release-rule contributions: scaled per-rule contrasts on observed data,
not Shapley values and not permutation-based.

## See also

[`gg_ivarpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_ivarpro.md).

## Examples

``` r
# \donttest{
if (requireNamespace("varPro", quietly = TRUE) &&
    requireNamespace("MASS", quietly = TRUE)) {
  set.seed(1)
  v <- varPro::varpro(medv ~ ., data = MASS::Boston, ntree = 50)
  plot(gg_ivarpro(v))
}

# }
```
