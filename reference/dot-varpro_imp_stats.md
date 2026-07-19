# Compute per-variable box statistics from a per-tree importance matrix

When `local.std = TRUE` the columns of `mat` are standardized to unit
variance before computing quantiles so that the display scale matches
the aggregate z-score. The `mean` column always stores raw
(unstandardized) column means.

## Usage

``` r
.varpro_imp_stats(mat, local.std = TRUE)
```
