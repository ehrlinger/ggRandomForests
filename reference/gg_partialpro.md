# Split varpro partial dependence data into continuous or categorical datasets

Takes the list returned by `varpro::partialpro` and separates variables
into two data frames: one for continuous predictors (parametric, non-
parametric, and causal effect curves) and one for categorical predictors
(one row per observation per category level).

## Usage

``` r
gg_partialpro(part_dta, nvars = NULL, cat_limit = 10, model = NULL)
```

## Arguments

- part_dta:

  partial plot data from `varpro::partialpro`. Each element of the list
  must contain fields `xvirtual`, `xorg`, `yhat.par`, `yhat.nonpar`, and
  `yhat.causal`.

- nvars:

  how many variables (list elements) to process. Defaults to all
  variables in `part_dta`.

- cat_limit:

  Variables with `length(xvirtual)` \\\le\\ `cat_limit` are treated as
  categorical. Default `10`.

- model:

  a label applied to all rows. Useful when combining results from
  multiple models in a single figure.

## Value

A named list with two elements:

- continuous:

  data.frame with columns `variable`, `parametric`, `nonparametric`,
  `causal`, `name` (and optionally `model`)

- categorical:

  data.frame with the same columns but one row per observation per
  category level

## Details

The split is governed by `cat_limit`: a variable is treated as
continuous when `length(xvirtual) > cat_limit`; otherwise it is treated
as categorical and the per-category rows are stacked.

## See also

[`gg_partial`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial.md)
[`varpro_feature_names`](https://ehrlinger.github.io/ggRandomForests/reference/varpro_feature_names.md)

## Examples

``` r
## Construct mock varpro partialpro output:
##   - "age": a continuous predictor (xvirtual has > 10 points)
##   - "sex": a categorical predictor (xvirtual has 2 points)
set.seed(42)
n_obs <- 30   # number of observations (rows in yhat matrices)
n_pts <- 15   # number of evaluation points for continuous variables

mock_data <- list(
  age = list(
    # xvirtual: evaluation grid for the marginal effect curve
    xvirtual   = seq(30, 80, length.out = n_pts),
    # xorg: original observed values (used only for categorical detection)
    xorg       = sample(seq(30, 80, by = 5), n_obs, replace = TRUE),
    # yhat matrices: n_obs rows x n_pts columns (predictions at each grid pt)
    yhat.par   = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
    yhat.nonpar = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
    yhat.causal = matrix(rnorm(n_obs * n_pts), nrow = n_obs)
  ),
  sex = list(
    # Two categories: the xvirtual grid has only 2 points
    xvirtual   = c(0, 1),
    xorg       = sample(c(0, 1), n_obs, replace = TRUE),
    # Two-column yhat matrices (one column per category)
    yhat.par   = matrix(rnorm(n_obs * 2), nrow = n_obs),
    yhat.nonpar = matrix(rnorm(n_obs * 2), nrow = n_obs),
    yhat.causal = matrix(rnorm(n_obs * 2), nrow = n_obs)
  )
)

result <- gg_partialpro(mock_data)

## Continuous result: one row per evaluation grid point
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

## Categorical result: n_obs rows per category level
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
