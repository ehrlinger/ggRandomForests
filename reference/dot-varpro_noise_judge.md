# Judge each row against tested noise of its own kind

Shared by
[`varpro_coverage()`](https://ehrlinger.github.io/ggRandomForests/reference/varpro_coverage.md)
and
[`gg_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md).
`out` needs `z`, `tested` and `coverage`; `vars` gives each row's
original variable, which the `noise` pattern and the kind are read from.
A row counts as tested here only when at least one tree scored it as
well as `coverage >= trim`, so `trim = 0` cannot make an unscored noise
row a bar at 0. Adds `noise`, `kind`, `noise_cut` and `noise_ref` (the
kind of the noise that set the bar, `NA` when none was tested), and sets
`selected`.

## Usage

``` r
.varpro_noise_judge(out, vars, object, noise, kind_values)
```
