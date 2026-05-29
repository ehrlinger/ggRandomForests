# Build per-tree importance matrix from varpro object results

Reconstructs the ntree x p per-tree importance matrix from
`object$results`, replicating the aggregation performed internally by
`varPro:::.importance.varpro.workhorse`.

## Usage

``` r
.build_varpro_imp_tree(object)
```
