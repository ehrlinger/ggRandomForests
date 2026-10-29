# Check `noise` and build the step that judges `$imp` against it

Returns `NULL` without `noise`; otherwise a function applied to the full
importance table, before `nvar` truncation, so the noise bar is judged
on every variable.

## Usage

``` r
.varpro_noise_annotator(object, noise, kind_values, local.std)
```
