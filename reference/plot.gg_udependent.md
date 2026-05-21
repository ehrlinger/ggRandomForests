# Plot a `gg_udependent` variable dependency graph

Renders the variable dependency graph from a `gg_udependent` object as a
ggraph network plot. Nodes are coloured by selection status; edge width
and opacity reflect dependency strength.

## Usage

``` r
# S3 method for class 'gg_udependent'
plot(x, layout = "fr", ...)
```

## Arguments

- x:

  A `gg_udependent` object from
  [`gg_udependent`](https://ehrlinger.github.io/ggRandomForests/reference/gg_udependent.md).

- layout:

  Character; igraph/ggraph layout algorithm. Common choices: `"fr"`
  (Fruchterman-Reingold, default), `"kk"` (Kamada-Kawai), `"stress"`,
  `"circle"`, `"grid"`.

- ...:

  Not currently used.

## Value

A `ggplot` object (built via ggraph).

## Details

Requires the ggraph package (`Suggests`). Install it with
`install.packages("ggraph")`.

Node colour: blue (`#4e8fcd`) for signal variables (`selected = TRUE`),
grey (`#888888`) otherwise. Node size scales with degree. Edge width and
opacity scale with raw dependency weight (`I[i,j]`).

## See also

[`gg_udependent`](https://ehrlinger.github.io/ggRandomForests/reference/gg_udependent.md)

## Examples

``` r
# \donttest{
if (requireNamespace("ggraph", quietly = TRUE)) {
  set.seed(42)
  uv <- varPro::uvarpro(iris[, -5], ntree = 50)
  plot(gg_udependent(uv))
}

# }
```
