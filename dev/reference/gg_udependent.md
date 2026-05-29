# Variable dependency graph from a uvarpro model

A `uvarpro` fit records how strongly each variable depends on the
others. This function pulls those cross-variable dependency scores from
the fit with
[`get.beta.entropy`](https://www.randomforestsrc.org/reference/utilities_internal.html)
and
[`sdependent`](https://www.randomforestsrc.org/reference/utilities_internal.html),
and returns them as a tidy list that `plot.gg_udependent` can draw as a
network.

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

  Numeric; the positive dependency threshold passed on to
  `sdependent()`. An edge \\i \to j\\ is drawn when
  `I[i, j] >= threshold`. Default `0.25`.

- q.signal:

  Quantile threshold (0–1) for picking out the signal variables; passed
  on to `sdependent()`. Default `0.75`.

- directed:

  Logical; `TRUE` (default) builds a directed igraph.

- min.degree:

  Integer or `NULL`. When set, only nodes with degree \\\ge\\
  `min.degree` are kept in `$nodes`, `$edges`, and `$graph`.

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

## What cross-variable dependency is doing

UVarPro (Zhou, Lu and Ishwaran, 2026) extends the varpro framework to
the unsupervised setting: grow a forest without a response, then use the
same region-release contrasts varpro uses for supervised importance to
ask, "which variables explain the structure in the data?" The
lasso-driven variant frames each region-release contrast as a
classification task — does an observation belong to the region or to its
release? — and fits a lasso logistic regression with the other variables
as predictors. The coefficient on variable \\j\\ in the model for
variable \\i\\'s region-release contrast is the entry \\I\[i, j\]\\ of
the matrix
[`varPro::get.beta.entropy()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
returns.

Read that entry as "how much does knowing \\j\\ help separate \\i\\'s
region from its release". A large \\I\[i, j\]\\ says \\j\\ carries
information about the structure varpro picked up in \\i\\.
[`varPro::sdependent`](https://www.randomforestsrc.org/reference/utilities_internal.html)
thresholds that matrix at a user-chosen cut and returns the set of
"signal" variables — the nodes with high enough out-degree to be worth
keeping. We pass the threshold through to `sdependent` and use the same
matrix to weight the edges of the resulting graph.

The graph is directed by default because \\I\[i, j\]\\ and \\I\[j, i\]\\
are separate lasso coefficients and need not agree; setting
`directed = FALSE` collapses each pair by taking the larger of the two,
which is appropriate when you only want to see that two variables are
dependent, not which way the dependency reads.

## What's in the output

`$edges` has one row per surviving edge with the raw weight `I[i, j]`
(or, for undirected graphs, the max of the two directions). `$nodes` has
one row per surviving variable with its degree (out-degree for directed,
total degree for undirected) and a `selected` flag for membership in the
`sdependent` signal set. `$graph` is the same information packaged as an
`igraph` object, with `weight`, `degree`, and `selected` attached so
`plot.gg_udependent` can render it without recomputing anything.

## What you use this for

- screen a wide unsupervised dataset for the small set of variables
  UVarPro thinks are carrying the signal — the nodes with high degree,
  or those flagged `selected = TRUE`;

- spot clusters of mutually dependent variables (hubs and the spokes
  around them) that may be measuring the same underlying construct;

- compare two datasets, or two preprocessing pipelines, by looking at
  how their dependency graphs change.

An edge in this graph is a statistical dependency in the unsupervised
decomposition of the data — it is not a causal arrow. A high \\I\[i,
j\]\\ says \\j\\ predicts \\i\\'s region membership, not that \\j\\
causes \\i\\.

## References

Zhou, L., Lu, M. and Ishwaran, H. (2026). Variable priority for
unsupervised variable selection. *Pattern Recognition*, 172:112727.

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
