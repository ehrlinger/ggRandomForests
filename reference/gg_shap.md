# SHAP (Shapley additive explanations) data object

`gg_shap` computes SHAP values for a
[`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html) or
[`randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
regression or classification forest by wrapping
[`kernelshap`](https://rdrr.io/pkg/kernelshap/man/kernelshap.html), and
reshapes them into a tidy data set with one row per (observation,
variable).

## Usage

``` r
gg_shap(object, newdata, bg_n = 50, which.class = 1, ...)
```

## Arguments

- object:

  A [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html) or
  [`randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  object (regression or classification).

- newdata:

  Optional `data.frame` of predictor values to explain (same columns as
  the model's training predictors). When missing, the model's own
  training predictors are used.

- bg_n:

  Size of the background/reference sample drawn from the training
  predictors and passed to
  [`kernelshap`](https://rdrr.io/pkg/kernelshap/man/kernelshap.html) as
  `bg_X`. Larger values are more accurate but slower.

- which.class:

  For classification forests, the class (integer column index into the
  predicted-probability matrix) whose predicted probability is
  explained. Defaults to 1.

- ...:

  Passed through to
  [`kernelshap`](https://rdrr.io/pkg/kernelshap/man/kernelshap.html)
  (e.g. `seed`, `exact`, `max_iter`).

## Value

A `gg_shap` object: a `data.frame` with columns `id` (observation
index), `vars` (variable name, an ordered factor ranked by mean absolute
SHAP), `shap` (the signed SHAP contribution), `value` (numeric feature
value, `NA` for categorical features), and `value_label` (feature value
as character). The background-sample mean prediction is stored in the
`"baseline"` attribute.

## See also

[`kernelshap`](https://rdrr.io/pkg/kernelshap/man/kernelshap.html)

## Examples

``` r
# \donttest{
if (requireNamespace("kernelshap", quietly = TRUE)) {
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
                               ntree = 50)
  gg_dta <- gg_shap(rf, bg_n = 20)
}
# }
```
