# Build the tidy importance data structures from importance() output

Build the tidy importance data structures from importance() output

## Usage

``` r
.build_varpro_imp_dfs(
  imp_out,
  imp_tree_mat,
  family,
  cutoff,
  nvar,
  faithful,
  conditional,
  coverage = NULL,
  trim = 0.1,
  annotate = NULL
)
```

## Value

A list with elements `imp`, `imp_tree`, `stats` and `conditional`.
