# Compute per-variable box statistics from a per-tree importance matrix

Quantiles of the per-tree values as they are. With varPro's
`local.std = TRUE` those values are already on z's scale (z is their
winsorised mean); with `FALSE` they are raw importances.

## Usage

``` r
.varpro_imp_stats(mat)
```

## Value

A data frame with one row per variable: `variable`, `median`, `q05`,
`q15`, `q85`, `q95` and `mean`.
