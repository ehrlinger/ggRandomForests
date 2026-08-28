# ggRandomForests v2.8.0 — varPro Phase 3 Design
## `gg_udependent`: Variable Dependency Graph for uvarpro Objects

- **Date:** 2026-05-20
- **Status:** Approved design (pre-implementation)
- **Version bump:** `2.7.3.9003 → 2.7.3.9004`
- **Branch:** `feat/varpro-phase3-gg-udependent` (created)
- **Author:** John Ehrlinger (design partner: Claude)
- **Vault mirror:** `~/Documents/ObsidianVault/R packages/ggRandomForests varPro Integration Design (2026-05-18).md` §7

---

## 1. Scope

Phase 3 of the v2.8.0 varPro integration cycle. Prerequisite: Phase 2
(`feat/varpro-phase2-gg-varpro`, PR #85) merged to `main`.

**This phase delivers:**
- `gg_udependent()`: extractor wrapping `varPro::sdependent()` on a `uvarpro` fit
- `plot.gg_udependent()`: ggraph network graph (nodes = variables, edges = dependency
  strength), with node/edge aesthetics driven by tidy provenance data
- `autoplot`, `print`, `summary` S3 companions
- `ggraph` added to `Suggests:`
- Version bump to `2.7.3.9004`

**Out of scope for this phase:** `ivarpro`, `isopro`, `beta.varpro` wrappers;
survival-family uvarpro (if applicable); any model-fitting code.

---

## 2. Architecture

### Input flow

```
uvarpro fit  →  gg_udependent()  →  sdependent(plot=FALSE)
                    │
                    ├── $edges   (variable_from, variable_to, weight)
                    ├── $nodes   (variable, degree, selected)
                    ├── $graph   (igraph object)
                    └── attr(,"provenance")
                                      │
                              plot.gg_udependent()  →  ggraph plot
```

`gg_udependent()` is responsible for all computation. `plot.gg_udependent()` is
pure rendering — no re-computation.

### Key design decisions

| Decision | Choice | Rationale |
|---|---|---|
| Input type | `uvarpro` fit | Matches `gg_varpro(varpro_fit)` convention |
| sdependent call | Internal, `plot=FALSE` | Hides varPro internals from user |
| Return value | tidy frames + igraph object | Lets power users manipulate the graph |
| Renderer | ggraph | Expressive; igraph already in Suggests |
| ggraph dependency | `Suggests:` only | Consistent with igraph placement; checked at plot time |

---

## 3. Extractor Signature

```r
gg_udependent(
  object,              # uvarpro fit (required)
  threshold  = 0.25,   # dependency threshold; passed to sdependent()
  q.signal   = 0.75,   # signal quantile; passed to sdependent()
  directed   = TRUE,   # directed graph? passed to sdependent()
  min.degree = NULL,   # if non-NULL, retain only nodes with degree >= min.degree
  ...                  # additional args forwarded to sdependent()
)
```

### Validation

- `object` must be present and inherit class `"uvarpro"` — stop with informative
  message otherwise
- `threshold` must be a single numeric in (0, 1) — stop if not
- `directed` must be scalar logical — stop if not

---

## 4. Return Value

A named list of class `c("gg_udependent", "list")`:

### `$edges`

Data frame with one row per directed edge (or undirected pair when
`directed=FALSE`):

| column | type | description |
|---|---|---|
| `variable_from` | character | source variable name |
| `variable_to` | character | target variable name |
| `weight` | double | dependency strength (from sdependent output) |

### `$nodes`

Data frame with one row per variable that appears in at least one edge:

| column | type | description |
|---|---|---|
| `variable` | factor | variable name; levels ordered by descending degree |
| `degree` | integer | total degree (in + out for directed, degree for undirected) |
| `selected` | logical | `degree >= 1` (or `>= min.degree` if supplied) |

### `$graph`

igraph object built from `$edges` with `igraph::graph_from_data_frame()`.
Directed/undirected matches the `directed` argument.  Node attribute
`selected` is stored on the graph for use by the plot method.

### `attr(., "provenance")`

Named list:

```r
list(
  threshold  = threshold,
  q.signal   = q.signal,
  directed   = directed,
  min.degree = min.degree,
  xvar.names = object$xvar.names,
  n          = nrow(object$x)  # or equivalent
)
```

---

## 5. Implementation Details

### Calling `sdependent()`

`sdependent()` signature (from varPro):

```r
sdependent(I, threshold=0.25, layout="grid", q.signal=0.75,
           directed=TRUE, min.degree=NULL, title=..., plot=TRUE)
```

Key:
- Pass `plot = FALSE` to suppress base-graphics output
- Pass `layout = "none"` (or equivalent) if the argument controls layout
  computation; otherwise any layout string is fine since we discard the base plot
- The return value must be inspected during Task 0 of the implementation plan to
  determine its structure (likely an igraph object or adjacency matrix)

### Building tidy frames from sdependent output

**If sdependent returns an igraph object:**

```r
g    <- sdependent_result
edge_df <- igraph::as_data_frame(g, what = "edges")
node_df <- igraph::as_data_frame(g, what = "vertices")
# rename to match schema above
```

**If sdependent returns an adjacency matrix:**

```r
adj  <- sdependent_result
g    <- igraph::graph_from_adjacency_matrix(adj, mode = if (directed) "directed" else "undirected",
                                             weighted = TRUE)
edge_df <- igraph::as_data_frame(g, what = "edges")
```

The implementation task includes a discovery step that prints `str(sdependent(..., plot=FALSE))`
on a small `uvarpro` fit before writing any production code.

### `min.degree` filtering

After building `$nodes`, if `min.degree` is non-NULL:

```r
keep <- nodes$degree >= min.degree
nodes$selected <- keep
# subset edges to only those between kept nodes
edges <- edges[edges$variable_from %in% nodes$variable[keep] &
               edges$variable_to   %in% nodes$variable[keep], ]
# rebuild igraph
graph <- igraph::graph_from_data_frame(edges, directed = directed,
                                        vertices = nodes[keep, ])
```

---

## 6. Plot Method

```r
plot.gg_udependent(x, layout = "fr", ...)
```

### Validation

- Checks `requireNamespace("ggraph", quietly = TRUE)` — stops with
  `"Install the 'ggraph' package to use plot.gg_udependent()"` if missing
- `layout` is passed directly to `ggraph::ggraph()`; valid values include
  `"fr"` (Fruchterman-Reingold), `"kk"` (Kamada-Kawai), `"stress"`, `"grid"`,
  `"circle"`, `"tree"`.

### Geometry

```r
ggraph::ggraph(x$graph, layout = layout) +
  ggraph::geom_edge_link(
    ggplot2::aes(width = weight, alpha = weight),
    color = "#4e8fcd"
  ) +
  ggraph::geom_node_point(
    ggplot2::aes(color = selected, size = degree)
  ) +
  ggraph::geom_node_label(
    ggplot2::aes(label = name),
    repel = TRUE, size = 3
  ) +
  ggplot2::scale_color_manual(
    values = c("TRUE" = "#4e8fcd", "FALSE" = "#888888"),
    guide = "none"
  ) +
  ggraph::scale_edge_width(range = c(0.5, 2.5), guide = "none") +
  ggraph::scale_edge_alpha(range = c(0.3, 1.0),  guide = "none") +
  ggplot2::labs(
    size = "Degree",
    caption = paste0("Dependency threshold: ", prov$threshold,
                     ". Layout: ", layout, ".")
  ) +
  ggplot2::theme_void()
```

### Returns

A `ggplot` object (via ggraph).

---

## 7. S3 Companions

### `print.gg_udependent(x, ...)`

```
<gg_udependent>  n=<n>  p=<|xvar.names|>  threshold=<threshold>
  Edges: <nrow(edges)>  Nodes in graph: <nrow(nodes)>  Selected: <sum(selected)>/<nrow(nodes)>
```

Returns `x` invisibly.

### `summary.gg_udependent(object, ...)`

Returns a `summary.gg_udependent` list containing `$edges`, `$nodes`, and
`$provenance`. Prints the node table (degree + selected) and edge table. Returns
object invisibly.

### `autoplot.gg_udependent(object, ...)`

Thin wrapper: `plot.gg_udependent(object, ...)`.

---

## 8. Test Plan

File: `tests/testthat/test_gg_udependent.R`

### Helpers

```r
make_uvp <- function(ntree = 25L) {
  set.seed(42L)
  varPro::uvarpro(iris[, -5], ntree = ntree)
}
```

### Input validation

- Missing object → stop
- Non-uvarpro object → stop ("uvarpro")
- threshold outside (0,1) → stop ("threshold")

### Class & structure

- Returns `gg_udependent` class
- `$edges` has columns `variable_from`, `variable_to`, `weight`
- `$nodes` has columns `variable`, `degree`, `selected`; `variable` is a factor
- `$graph` is an igraph object
- `$graph` directed/undirected matches `directed` argument
- `$edges` is empty data frame (not NULL) when no dependencies detected

### Provenance

- `attr(gg, "provenance")` is a list with all expected fields
- Provenance `threshold` matches argument

### min.degree filtering

- `min.degree=2` retains only nodes with degree ≥ 2
- `$edges` contains only edges between retained nodes

### Plot smoke tests

- `plot(gg_udependent(uvp))` returns a ggplot (requires ggraph installed)
- `layout="kk"` also returns a ggplot
- Missing ggraph → informative error (mock `requireNamespace` returning FALSE)

### S3 companions

- `autoplot(gg)` returns ggplot
- `print(gg)` returns object invisibly; output contains "gg_udependent"
- `summary(gg)` returns `summary.gg_udependent`

---

## 9. Files

| Action | Path |
|---|---|
| Create | `R/gg_udependent.R` |
| Create | `R/plot.gg_udependent.R` |
| Create | `tests/testthat/test_gg_udependent.R` |
| Modify | `DESCRIPTION` (add `ggraph` to `Suggests:`) |
| Modify | `NEWS.md` (add Phase 3 entry) |
| Auto-generate | `man/gg_udependent.Rd`, `man/plot.gg_udependent.Rd` |
| Snapshot | `tests/testthat/_snaps/` (vdiffr, if ggraph available in CI) |

---

## 10. Dependency Notes

- **`igraph`**: already in `Suggests:` (Phase 0, PR #79). Used for graph
  construction and data extraction.
- **`ggraph`**: add to `Suggests:`. Checked at runtime in `plot.gg_udependent()`
  via `requireNamespace("ggraph", quietly = TRUE)`. Not needed for extraction.
- Both packages are already widely available in the R ecosystem; no unusual
  install burden.

---

## 11. Out of Scope

- Survival-family uvarpro (unknown if sdependent applies — deferrable)
- Interactive/Shiny graph output
- Export of the igraph object to graphviz/DOT format
- Custom node/edge color scales via `...` passthrough (can be added in a patch)
