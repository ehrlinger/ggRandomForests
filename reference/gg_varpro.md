# Variable importance data from a varPro model

Pulls the per-tree importance scores out of a fitted `varpro` object and
summarises them into a data structure the plot method can draw as a
boxplot. The box hinges are the 15th and 85th percentiles and the
whiskers run to the 5th and 95th – not the usual Tukey 1.5 IQR whiskers.
For a classification forest you can also keep the class-conditional
importances.

## Usage

``` r
gg_varpro(
  object,
  local.std = TRUE,
  cutoff = 0.79,
  faithful = FALSE,
  conditional = FALSE,
  nvar = NULL,
  ...
)
```

## Arguments

- object:

  A fitted `varpro` object (required).

- local.std:

  Logical; default `TRUE`. When `TRUE` the per-tree importances are put
  on the z-scale before the box statistics are computed. Set it `FALSE`
  to keep the raw importance scale, which is what `type = "raw"` in
  `plot.gg_varpro` needs.

- cutoff:

  Numeric; the z-score above which a variable is treated as selected.
  Default `0.79`. A variable with aggregate z above the cutoff is
  flagged `selected = TRUE` in `$imp`.

- faithful:

  Logical; default `FALSE`. When `TRUE`, `$imp.tree` is kept so
  `plot.gg_varpro` can scatter the per-tree points over the box.

- conditional:

  Logical; default `FALSE`. When `TRUE`, and only for a classification
  forest, the `$conditional.z` matrix is extracted and stored as
  `$conditional`.

- nvar:

  Integer; keep only the top `nvar` variables, ranked by median per-tree
  z, after the cutoff filter has been applied. `NULL` keeps all of them.

- ...:

  Additional arguments passed to
  [`varPro::importance()`](https://www.randomforestsrc.org/reference/importance.html).

## Value

A named list of class `"gg_varpro"` with elements:

- `$imp`:

  Summary data frame: `variable` (factor with levels ordered by
  descending median per-tree z), `z` (aggregate z-score from
  `importance()`), `selected` (logical, `z > cutoff`).

- `$imp.tree`:

  `NULL` when `faithful = FALSE`; otherwise an ntree x p matrix of
  per-tree importance values.

- `$stats`:

  Per-variable summary: `variable`, `median`, `q05`, `q15`, `q85`, `q95`
  (on z-scale when `local.std = TRUE`, raw when `FALSE`), plus `mean`
  (raw importance mean, always stored).

- `$conditional`:

  `NULL` when `conditional = FALSE`; otherwise a data frame with columns
  `variable`, `class`, `z` (one row per variable x class combination).

A `"provenance"` attribute carries `family`, `local.std`, `cutoff`,
`faithful`, `conditional`, `xvar.names`, and `n`.

## See also

[`plot.gg_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_varpro.md),
[`gg_vimp`](https://ehrlinger.github.io/ggRandomForests/reference/gg_vimp.md)

## Examples

``` r
# \donttest{
set.seed(42)
vp <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)
gg <- gg_varpro(vp)
print(gg)
#> <gg_varpro>  family: regr  |  n: 32  |  family: regr  |  cutoff: 0.79  |  faithful: FALSE
#>   2 of 4 variables selected (z > 0.79)
plot(gg)

# }
```
