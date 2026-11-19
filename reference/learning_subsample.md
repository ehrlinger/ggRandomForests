# Run the subsamples for a learning curve

Calls
[`randomForestSRC::subsample()`](https://www.randomforestsrc.org//reference/subsample.rfsrc.html)
once per fraction, with `performance = TRUE`, and optionally once more
as a double bootstrap for the whole cohort, and returns the list that
[`gg_learning()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_learning.md)
tidies. It records the `stratify` setting on each result, which
[`subsample()`](https://www.randomforestsrc.org//reference/subsample.rfsrc.html)
itself does not keep, so
[`gg_learning()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_learning.md)
can report the number of events in each refit as exact rather than
assumed.

## Usage

``` r
learning_subsample(
  object,
  fractions = c(0.125, 0.25, 0.5),
  replicates = 20L,
  bootstrap = TRUE,
  stratify = TRUE,
  ...
)
```

## Arguments

- object:

  A `randomForestSRC` forest (class `rfsrc`), grown with the settings
  the refits should copy.

- fractions:

  Fractions of the cohort to refit on, each greater than 0 and less than
  1.

- replicates:

  Positive whole number; refits per fraction, passed to
  [`subsample()`](https://www.randomforestsrc.org//reference/subsample.rfsrc.html)
  as `B`.

- bootstrap:

  Logical; also run the double bootstrap for the whole cohort (fraction
  1). Defaults to `TRUE`. Left out, with a warning, when `object`
  imputed missing values (`na.action = "na.impute"` on data with any
  missing value): `randomForestSRC` crashes R double-bootstrapping such
  a forest. The subsamples are unaffected.

- stratify:

  Logical; passed to
  [`subsample()`](https://www.randomforestsrc.org//reference/subsample.rfsrc.html),
  which samples within each class (or survival status) when `TRUE`, its
  default.

- ...:

  Further arguments to
  [`subsample()`](https://www.randomforestsrc.org//reference/subsample.rfsrc.html),
  such as `importance`.

## Value

A list of
[`subsample()`](https://www.randomforestsrc.org//reference/subsample.rfsrc.html)
results, one per fraction in increasing order and the bootstrap last
(when it is run), each carrying a `stratify` attribute.

## Details

This is the expensive step: it grows `replicates` forests per fraction,
plus as many for the bootstrap, each the size of `object`. Run it once,
save the result and pass it to
[`gg_learning()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_learning.md)
as often as needed.

[`subsample()`](https://www.randomforestsrc.org//reference/subsample.rfsrc.html)
draws its subsamples with R's random number generator, so set the seed
before calling this function for a reproducible curve.

## See also

[`gg_learning()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_learning.md),
[`randomForestSRC::subsample()`](https://www.randomforestsrc.org//reference/subsample.rfsrc.html).

## Examples

``` r
# \donttest{
fit <- randomForestSRC::rfsrc(mpg ~ ., data = mtcars, ntree = 50,
                              importance = "permute", seed = -1)
set.seed(1)
subsamples <- learning_subsample(fit, fractions = c(0.25, 0.5),
                                 replicates = 5)
curve <- gg_learning(subsamples, top_k = 3, min_events = 10)
summary(curve)
#>   fraction  n events   counts B error_mean  error_sd jaccard     kendall
#> 1     0.25  8      8    exact 5   35.78079 22.852897    0.68 -0.63951955
#> 2     0.50 16     16    exact 5   13.80185  4.501053    0.38 -0.02000000
#> 3     1.00 32     32 expected 5   10.88033  1.889053    0.49  0.02666667
#>   low_events optimistic
#> 1       TRUE      FALSE
#> 2      FALSE      FALSE
#> 3      FALSE      FALSE
# }
```
