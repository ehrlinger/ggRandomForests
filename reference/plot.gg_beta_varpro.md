# Plot a `gg_beta_varpro` object

Horizontal bar chart of `mean(|β̂|)` per variable, sorted descending so
the eye lands on the top variable first. Bars filled blue when above the
selection cutoff, grey otherwise. Dashed red line marks the cutoff.

## Usage

``` r
# S3 method for class 'gg_beta_varpro'
plot(x, ...)
```

## Arguments

- x:

  A `gg_beta_varpro` object from
  [`gg_beta_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_varpro.md).

- ...:

  Not currently used.

## Value

A `ggplot` object.

## Reading the chart

Each bar is the average magnitude of a per-rule lasso coefficient for
that variable. **The numeric scale carries the predictor's units** — if
"age" is in years and "creatinine" is in mg/dL, a longer bar for age
does not mean age is "more important" in any unit-free sense.
Comparisons across data sets or across variables with very different
units require keeping the units context in mind. Within one data set,
bars are comparable up to that unit caveat.

Variables above the cutoff are coloured blue and flagged `selected`;
variables below are grey. Lasso shrinkage can drive a rule's β̂ to
exactly zero — those rules are kept in the average, so a variable with
many shrunk-to-zero rules will sit lower in the ranking than one whose
released coefficients are consistently non-zero.

## What this tells you

Use the bar chart as a selection ranking, not as an effect-size axis.
Pair it with
[`gg_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md)
to see where split-strength importance and local lasso-β importance
agree or disagree; disagreement is often the interesting signal.

## See also

[`gg_beta_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_varpro.md).

## Examples

``` r
# \donttest{
if (requireNamespace("varPro", quietly = TRUE)) {
  set.seed(1)
  v <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)
  plot(gg_beta_varpro(v))
}

# }
```
