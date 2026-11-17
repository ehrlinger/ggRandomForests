# Plot a random forest learning curve

Draws one panel of a
[`gg_learning()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_learning.md)
curve against the number of events (observations, for regression). The
default `"stability"` panel shows the top-k Jaccard index and Kendall
rank correlation of the VIMP ranking across replicates; the `"error"`
panel shows each replicate's OOB error with the mean at each fraction.
Points flagged `low_events` or `optimistic` are drawn open, because both
make the curve look worse or better than a fresh cohort of that size
would. Unstratified refits, whose counts are unknown, are drawn as
crosses and the axis says their counts are expected. The double
bootstrap of the whole cohort is drawn as a square: it draws with
replacement, so its x value is the cohort's count, the number expected
in each draw rather than an exact one, and the square is open when that
count is below `min_events`.

## Usage

``` r
# S3 method for class 'gg_learning'
plot(x, panel = c("stability", "error"), ...)
```

## Arguments

- x:

  A `gg_learning` object from
  [`gg_learning()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_learning.md).

- panel:

  Which curve to draw: `"stability"` (default) or `"error"`.

- ...:

  Not used.

## Value

A `ggplot` object.

## See also

[`gg_learning()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_learning.md).
