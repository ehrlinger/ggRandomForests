# Stop when too many real variables went untested

`out` is the coverage table before sorting, with `noise` only when a
noise pattern was given. Testedness is judged per term from `term_ok`,
not from `out$tested`: a factor's rolled-up coverage is the union of its
levels' trees, so it can pass `trim` while every level, and so every
level's z, was trimmed away. Returns `invisible(NULL)` when the screen
passes.

## Usage

``` r
.varpro_coverage_guard(out, max_untested, trim, term_var, term_ok)
```
