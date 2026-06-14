# Per-variable lasso-beta importance from an unsupervised varPro fit

Tidy wrapper around
[`varPro::get.beta.entropy()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
for a `uvarpro` object. Where
[`gg_beta_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_varpro.md)
refines the *supervised* release-rule contrast, `gg_beta_uvarpro()` does
the unsupervised analogue: `uvarpro()` builds entropy regions with no
response, and `get.beta.entropy()` fits a cross-validated lasso within
each region to ask how strongly every other variable explains the
released variable. Averaging the absolute lasso coefficients per
variable gives one number per variable: an unsupervised, lasso-flavoured
importance.

## Usage

``` r
gg_beta_uvarpro(object, ..., cutoff = NULL, beta_fit = NULL)
```

## Arguments

- object:

  A `uvarpro` object from
  [`varPro::uvarpro()`](https://www.randomforestsrc.org/reference/uvarpro.html).

- ...:

  Forwarded to
  [`varPro::get.beta.entropy()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
  when `beta_fit = NULL` (e.g. `pre.filter`, `second.stage`, `use.cv`).
  Ignored, with a warning, when `beta_fit` is supplied.

- cutoff:

  Selection threshold on `beta_mean`. `NULL` (default) uses
  `mean(beta_mean)`; a scalar sets it explicitly. Variables at or above
  the cutoff are flagged `selected`.

- beta_fit:

  Optional pre-computed
  [`varPro::get.beta.entropy()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
  matrix for `object`. When supplied, must be a numeric matrix with
  column names (the variables); `...` is then ignored.

## Value

A `gg_beta_uvarpro` object (a `data.frame`), one row per variable,
most-important first, with columns:

- `variable`:

  factor; levels reversed so the most-important variable lands at the
  top after
  [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)
  (the `gg_vimp` convention).

- `beta_mean`:

  `mean(|lasso beta|)` over the released regions
  (`colMeans(beta, na.rm = TRUE)`).

- `n_released`:

  number of regions contributing a non-`NA` coefficient for the
  variable.

- `selected`:

  logical; `beta_mean >= cutoff`.

The `provenance` attribute records `source`, `family` (`"unsupv"`),
`cutoff`, `n_var`, `n_released_regions`, and `precomputed`.

## Details

`get.beta.entropy(o)` returns a (released-variable x variable) numeric
matrix of absolute lasso coefficients. The column mean (`na.rm = TRUE`)
is the per-variable importance reported here, matching the canonical
`sort(colMeans(beta, na.rm = TRUE), decreasing = TRUE)` idiom in the
[`varPro::uvarpro()`](https://www.randomforestsrc.org/reference/uvarpro.html)
help ("iowa housing - illustrates lasso importance").

Because `get.beta.entropy()` is expensive (a cross-validated `glmnet`
per region), the `beta_fit` argument accepts a pre-computed matrix so
you can iterate on the cutoff without re-fitting. The pairing mirrors
the `beta_fit` argument of
[`gg_beta_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_varpro.md).

## See also

[`gg_beta_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_varpro.md)
(supervised analogue),
[`gg_udependent()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_udependent.md),
[`varPro::get.beta.entropy()`](https://www.randomforestsrc.org/reference/utilities_internal.html),
[`varPro::uvarpro()`](https://www.randomforestsrc.org/reference/uvarpro.html).

## Examples

``` r
# \donttest{
if (requireNamespace("varPro", quietly = TRUE)) {
  set.seed(1)
  o <- varPro::uvarpro(mtcars, ntree = 50)
  gg <- gg_beta_uvarpro(o)
  plot(gg)
}

# }
```
