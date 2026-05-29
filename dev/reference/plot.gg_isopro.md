# Plot a varPro isolation-forest anomaly score

Renders a `gg_isopro` object as a ranked elbow (observations sorted by
anomaly score), a density of scores, or both side-by-side. Optionally
annotates a threshold either in score-space (`threshold`) or in
quantile-space (`top_n_pct`).

## Usage

``` r
# S3 method for class 'gg_isopro'
plot(
  x,
  panel = c("both", "elbow", "density"),
  threshold = NULL,
  top_n_pct = NULL,
  ...
)
```

## Arguments

- x:

  A `gg_isopro` object from
  [`gg_isopro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_isopro.md).

- panel:

  One of `"both"` (default — a `patchwork` of elbow + density),
  `"elbow"`, or `"density"` (each returns a single `ggplot`).

- threshold:

  Numeric in `[0, 1]`, or `NULL` (default). If set, draws a reference
  line at that `howbad` value on the elbow and density.

- top_n_pct:

  Numeric in `(0, 100)`, or `NULL` (default). If set, resolves to the
  matching `howbad` quantile and draws the same reference line. If both
  `threshold` and `top_n_pct` are supplied, `threshold` wins with a
  [`message()`](https://rdrr.io/r/base/message.html).

- ...:

  Currently unused.

## Value

A `ggplot` (single panel) or a `patchwork` (`panel = "both"`).

## Reading the elbow

The elbow plot is the canonical anomaly-detection picture. The x-axis is
observation rank — observations sorted from most ordinary to most
anomalous — and the y-axis is the `howbad` score. For a clean population
the curve sits flat near zero across the bulk of the data and then bends
sharply upward in the right tail; that bend is where the anomalous
observations live. The point of the plot is not to read off a single
score, it is to *see where the curve breaks*. Pick a cutoff there. Pass
it back in as `threshold` (for a score) or `top_n_pct` (for "the top 5\\
reference line so you can record the choice you made.

## Reading the density

The density panel is the same scores viewed as a distribution. A single
tight mode near zero with a long thin right tail is the picture you hope
for — bulk of the data ordinary, a few clear anomalies. A bimodal
density says you may have two populations rather than one clean cluster
plus outliers, and the cutoff question becomes harder. Either way, this
panel is a sanity check on what the elbow suggests.

## Comparing methods

When the input object carries a `method` column (because you bound
several
[`gg_isopro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_isopro.md)
calls together), both panels colour by method automatically. The point
of comparing `"rnd"`, `"unsupv"`, and `"auto"` is not to pick a winner
from the figure alone — it is to see whether the methods agree on which
observations are anomalous. Curves that overlap in the right tail and
elbow at roughly the same rank are telling you the same story three
ways. Curves that diverge are telling you the score is method-sensitive,
which is itself useful information.

## See also

[`gg_isopro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_isopro.md),
[`isopro`](https://www.randomforestsrc.org/reference/isopro.html)
