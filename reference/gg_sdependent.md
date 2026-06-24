# Signal-variable detection from an unsupervised varPro fit

Tidy wrapper around
[`varPro::sdependent()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
for a `uvarpro` object. Where
[`gg_udependent()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_udependent.md)
draws the cross-variable dependency *graph*, `gg_sdependent()` surfaces
`sdependent()`'s *signal-variable detection*: a ranked table of the
per-variable signal score and graph degree, with the variables flagged
as "signal" (those whose dependency structure clears the detection
threshold).

## Usage

``` r
gg_sdependent(
  object,
  ...,
  threshold = 0.25,
  q.signal = 0.75,
  directed = TRUE,
  min.degree = NULL,
  beta_fit = NULL
)
```

## Arguments

- object:

  A `uvarpro` object from
  [`varPro::uvarpro()`](https://www.randomforestsrc.org/reference/uvarpro.html).

- ...:

  Forwarded to
  [`varPro::get.beta.entropy()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
  when `beta_fit = NULL`; ignored, with a warning, when `beta_fit` is
  supplied.

- threshold, q.signal, directed, min.degree:

  Passed to
  [`varPro::sdependent()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
  (defaults match
  [`gg_udependent()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_udependent.md)).

- beta_fit:

  Optional precomputed
  [`varPro::get.beta.entropy()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
  matrix.

## Value

A `gg_sdependent` object (a `data.frame`), one row per candidate
variable, most-signal first, with columns:

- `variable`:

  factor; levels reversed so the top variable lands at the top after
  [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html).

- `imp_score`:

  `sdependent()` per-variable signal score.

- `degree`:

  node degree in the dependency graph.

- `signal`:

  logical; variable is in `sdependent()$signal.vars`.

The `provenance` attribute records `source`, `family` (`"unsupv"`),
`threshold`, `q.signal`, `directed`, `n_signal`, and `n_var`.

## Details

`sdependent()` runs on the
[`varPro::get.beta.entropy()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
lasso-coefficient matrix and returns, with `plot = FALSE`, a list of
`imp.score` (per-variable signal score), `degree` (node degree in the
dependency graph), and `signal.vars` (the detected signal set). This
wrapper tidies that into one row per candidate variable, ranked by
`imp.score`. Because the entropy matrix is the expensive part,
`beta_fit` accepts a precomputed
[`varPro::get.beta.entropy()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
matrix (shared with
[`gg_beta_uvarpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_uvarpro.md)
and
[`gg_udependent()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_udependent.md)).

## See also

[`gg_udependent()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_udependent.md)
(the dependency graph),
[`gg_beta_uvarpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_uvarpro.md)
(lasso importance),
[`varPro::sdependent()`](https://www.randomforestsrc.org/reference/utilities_internal.html),
[`varPro::uvarpro()`](https://www.randomforestsrc.org/reference/uvarpro.html).

## Examples

``` r
# \donttest{
if (requireNamespace("varPro", quietly = TRUE)) {
  set.seed(1)
  o <- varPro::uvarpro(mtcars, ntree = 50)
  gg <- gg_sdependent(o)
  plot(gg)
}

# }
```
