# Compute per-variable box statistics from a per-tree importance matrix

Quantiles of the per-tree values as they are. With varPro's
`local.std = TRUE` those values are already on z's scale (z is their
winsorised mean); with `FALSE` they are raw importances.

## Usage

``` r
.varpro_imp_stats(mat)
```
