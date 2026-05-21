# Variable dependency graph from a uvarpro model

Extracts cross-variable dependency scores from a fitted `uvarpro` object
using
[`get.beta.entropy`](https://www.randomforestsrc.org/reference/utilities_internal.html)
and
[`sdependent`](https://www.randomforestsrc.org/reference/utilities_internal.html),
and returns a tidy list suitable for `plot.gg_udependent`.

## Usage

``` r
gg_udependent(
  object,
  threshold = 0.25,
  q.signal = 0.75,
  directed = TRUE,
  min.degree = NULL,
  ...
)
```

## Arguments

- object:

  A fitted `uvarpro` object (required).

- threshold:

  Numeric; positive dependency threshold passed to `sdependent()`. An
  edge \\i \to j\\ is drawn when `I[i, j] >= threshold`. Default `0.25`.

- q.signal:

  Quantile threshold (0–1) for signal variable selection; passed to
  `sdependent()`. Default `0.75`.

- directed:

  Logical; `TRUE` (default) builds a directed igraph.

- min.degree:

  Integer or `NULL`. When non-`NULL`, only nodes with degree \\\ge\\
  `min.degree` are retained in `$nodes`, `$edges`, and `$graph`.

- ...:

  Additional arguments forwarded to
  [`varPro::sdependent()`](https://www.randomforestsrc.org/reference/utilities_internal.html).

## Value

A named list of class `"gg_udependent"` with elements:

- `$edges`:

  Data frame: `variable_from`, `variable_to`, `weight` (raw
  cross-importance value).

- `$nodes`:

  Data frame: `variable` (factor, levels by descending degree), `degree`
  (integer; out-degree when `directed = TRUE`, total degree when
  `directed = FALSE`), `selected` (logical, `TRUE` if in `sdependent`'s
  signal set).

- `$graph`:

  igraph object. `NULL` if no dependencies detected.

A `"provenance"` attribute carries `threshold`, `q.signal`, `directed`,
`min.degree`, `xvar.names`, and `n`.

## See also

[`plot.gg_udependent`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_udependent.md)

## Examples

``` r
# \donttest{
set.seed(42)
uv <- varPro::uvarpro(iris[, -5], ntree = 50)
gg <- gg_udependent(uv)
print(gg)
#> <gg_udependent>  n=150  p=4  threshold=0.25
#>   Edges: 6  Nodes in graph: 3  Selected: 1/3
# }
```
