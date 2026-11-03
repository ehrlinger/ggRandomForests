# The dashed selection line: the cutoff, or the noise bar

One noise bar (a single kind, or both kinds at the same z) is a line
across the plot; two are a dashed tick on each variable's row.

## Usage

``` r
.varpro_cut_layer(stats_df, cutoff, noise_bar)
```

## Value

A ggplot2 layer:
[`geom_hline()`](https://ggplot2.tidyverse.org/reference/geom_abline.html)
for one bar,
[`geom_errorbar()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html)
for one per row.
