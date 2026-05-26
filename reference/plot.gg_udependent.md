# Plot a `gg_udependent` variable dependency graph

Draws the dependency graph held in a `gg_udependent` object as a ggraph
network. Node colour marks whether a variable made the signal set, and
the width and opacity of an edge tell you how strong the dependency
between its two variables is.

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

  Character; the igraph/ggraph layout algorithm. Common choices are
  `"fr"` (Fruchterman-Reingold, the default), `"kk"` (Kamada-Kawai),
  `"stress"`, `"circle"`, and `"grid"`.

- ...:

  Not currently used.

## Value

A `ggplot` object (built via ggraph).

## Details

This plot needs the ggraph package, which is in `Suggests` rather than
installed for you. If it is missing, run `install.packages("ggraph")`.

A signal variable (`selected = TRUE`) gets a blue node (`#4e8fcd`); the
rest are grey (`#888888`). Node size grows with degree. Edge width and
opacity both grow with the raw dependency weight `I[i,j]`.

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
