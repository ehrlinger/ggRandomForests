# Split partial dependence data into continuous or categorical datasets

A partial dependence curve answers a what-if question about a forest:
hold every other predictor at its observed value, sweep one of them
across its range, and watch how the ensemble prediction moves.
Marginalized over the joint distribution of the other variables, the
resulting curve isolates the average effect of the swept predictor
alone.

## Usage

``` r
gg_partial(part_dta, nvars = NULL, cat_limit = 10, model = NULL)
```

## Arguments

- part_dta:

  partial plot data from `rfsrc::plot.variable`

- nvars:

  how many of the partial plot variables to calculate

- cat_limit:

  Categorical features are built when there are fewer than `cat_limit`
  unique feature values.

- model:

  a label name applied to all features. Useful when combining multiple
  partial plot objects in figures.

## Value

A named list with two elements:

- continuous:

  data.frame with columns `x`, `yhat`, `name` (and optionally `model`)
  for continuous variables

- categorical:

  data.frame with the same columns but with `x` as a factor, for
  low-cardinality / categorical variables

## Details

`gg_partial` handles the bookkeeping step after you've already called
`rfsrc::plot.variable(partial = TRUE)`: it takes the list that function
returns and separates the variables into two tidy data frames – one for
continuous predictors (plotted as lines) and one for categorical
predictors (plotted as bar charts). The split is controlled by
`cat_limit`: variables with more unique x-values than this threshold are
treated as continuous; all others are categorical.

If you'd rather skip the `plot.variable` step and pass the fitted forest
directly, see
[`gg_partial_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md),
which calls `partial.rfsrc` for you.

## Note

Partial-dependence extraction is `randomForestSRC`-only; there is no
`randomForest` method (the `randomForest` package provides no comparable
partial-dependence interface).

## See also

[`gg_partial_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
[`gg_partialpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)

## Examples

``` r
## Build a small regression forest on the airquality dataset
set.seed(42)
airq <- na.omit(airquality)
rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50)

## Compute partial dependence via plot.variable (show.plots = FALSE to
## suppress the base-graphics output, we only want the data)
pv <- randomForestSRC::plot.variable(rf, partial = TRUE,
                                      show.plots = FALSE)

## Split into continuous and categorical data frames
result <- gg_partial(pv)
head(result$continuous)
#> # A tibble: 6 × 3
#>       x  yhat name   
#>   <dbl> <dbl> <chr>  
#> 1     7  30.3 Solar.R
#> 2    14  30.4 Solar.R
#> 3    25  30.8 Solar.R
#> 4    44  31.3 Solar.R
#> 5    65  32.7 Solar.R
#> 6    81  36.0 Solar.R

## Label this model for later comparison with a second forest
result_labelled <- gg_partial(pv, model = "airq_model")
unique(result_labelled$continuous$model)
#> [1] "airq_model"
```
