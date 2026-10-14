# Share of scored trees with a usable release rule, per variable

A rule is usable when varPro scored it (non-missing `imp`). The
denominator is the trees in `scored$results`, the same trees the
per-tree matrix and z are built from.

## Usage

``` r
.varpro_imp_coverage(scored, trees)
```
