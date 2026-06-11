# Plot a `gg_varpro` variable importance object

Draws a horizontal boxplot of the per-tree importance z-scores, or of
the raw importances if you asked for those. Set `faithful = TRUE` at
extract time and the per-tree points are scattered over the box; for a
classification forest, `conditional = TRUE` splits the plot into one
facet per class.

## Usage

``` r
# S3 method for class 'gg_varpro'
plot(x, type, ...)
```

## Arguments

- x:

  A `gg_varpro` object from
  [`gg_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md).

- type:

  Character; the display scale. Leave it off and it is read from
  `provenance$local.std`: `"z"` when `local.std = TRUE` (the default),
  `"raw"` when `local.std = FALSE`. Asking for a scale that the extract
  step did not prepare raises an error.

- ...:

  Not currently used.

## Value

A `ggplot` object.

## Details

**Boxplot geometry:** the hinges are the 15th and 85th percentiles of
the per-tree z-distribution, and the whiskers run to the 5th and 95th.
This is **not** a Tukey boxplot, and the plot carries a caption that
says so.

**`faithful = TRUE`:** the per-tree values are jittered over the box as
semi-transparent points, on the same scale as the box itself (z when
`local.std = TRUE`, raw when `local.std = FALSE`). The box is drawn
faint to let the points show through, and a white-outlined dot marks the
mean.

**`conditional = TRUE`:** the class-conditional importances
(`$conditional`) are shown as a faceted bar chart
(`facet_wrap(~class, nrow = 1)`). Variables keep the sort order set by
the unconditional median z in `$stats`, so the facets line up.

## Reading the boxplot

Variables are sorted top to bottom by descending median per-tree
importance, so the eye lands on the most important variable first. For
each variable the box spans the 15th to 85th percentile of the per-tree
scores, the centre line is the median, and the whiskers run out to the
5th and 95th percentile, not the usual Tukey 1.5 IQR whiskers. The
dashed vertical line is the selection `cutoff` (default `0.79`). On the
default z-score axis (`local.std = TRUE`) that line is a z; on the
raw-importance axis (`local.std = FALSE`, `type = "raw"`) it is the same
numeric value but in raw-importance units. Boxes whose aggregate value
sits above the line are coloured blue and flagged `selected = TRUE`, the
rest are grey. A selected variable with a tight, high box is a variable
the forest agrees on across trees. A selected variable with a wide box
that straddles the cutoff is one to look at twice before relying on it.

With `faithful = TRUE` the box is drawn faint and the per-tree values
are jittered over it as semi-transparent points, on the same scale as
the box (z when `local.std = TRUE`, raw otherwise). A white-outlined dot
marks the mean. Use this view when you want to see how individual trees
voted rather than just the summary.

For a classification forest with `conditional = TRUE` the plot splits
into one facet per class. Variables keep the unconditional sort order,
so the rows line up across facets and you can read across to see which
class a variable is informative for.

## What this tells you

Take the variables above the cutoff as your candidate set. Use the width
of the box and the per-tree overlay to gauge confidence: a narrow box
well above the cutoff is a confident pick, a wide box that crosses it is
a coin flip you should not lean on. For classification, conditional
importance tells you which variables drive which class; a variable that
is unconditionally important but only important for one class out of
several is still useful, just useful for a narrower question.

## See also

[`gg_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md)

## Examples

``` r
# \donttest{
set.seed(42)
vp <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)
plot(gg_varpro(vp))

plot(gg_varpro(vp, faithful = TRUE))

# }
```
