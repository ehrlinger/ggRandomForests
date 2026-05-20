# Variable importance data from a varPro model

Extracts per-tree importance scores from a fitted `varpro` object,
summarises them into an honest boxplot-ready data structure (hinges at
15th/85th percentile, whiskers at 5th/95th), and optionally retains
class-conditional importance for classification forests.

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

  Logical; default `TRUE`. When `TRUE` the per-tree importances are
  normalised to z-scale before computing box statistics. Set to `FALSE`
  to retain the raw importance scale (required for `type = "raw"` in
  `plot.gg_varpro`).

- cutoff:

  Numeric z-score threshold for variable selection; default `0.79`.
  Variables with aggregate z \> cutoff are flagged `selected = TRUE` in
  `$imp`.

- faithful:

  Logical; default `FALSE`. When `TRUE`, `$imp.tree` is retained so
  `plot.gg_varpro` can overlay per-tree jitter points in
  `plot.gg_varpro`.

- conditional:

  Logical; default `FALSE`. When `TRUE` (classification forests only)
  extracts the `$conditional.z` matrix and stores it as `$conditional`.

- nvar:

  Integer; retain only the top `nvar` variables (by median per-tree z)
  after applying the cutoff filter. `NULL` keeps all.

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
