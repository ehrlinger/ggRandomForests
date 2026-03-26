# Plot a [`gg_vimp`](https://ehrlinger.github.io/ggRandomForests/reference/gg_vimp.md) object, extracted variable importance of a [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html) object

Plot a
[`gg_vimp`](https://ehrlinger.github.io/ggRandomForests/reference/gg_vimp.md)
object, extracted variable importance of a
[`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html) object

## Usage

``` r
# S3 method for class 'gg_vimp'
plot(x, relative, lbls, ...)
```

## Arguments

- x:

  [`gg_vimp`](https://ehrlinger.github.io/ggRandomForests/reference/gg_vimp.md)
  object created from a
  [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html)
  object

- relative:

  should we plot vimp or relative vimp. Defaults to vimp.

- lbls:

  A vector of alternative variable labels. Item names should be the same
  as the variable names.

- ...:

  optional arguments passed to gg_vimp if necessary

## Value

`ggplot` object

## References

Breiman L. (2001). Random forests, Machine Learning, 45:5-32.

Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R,
Rnews, 7(2):25-31.

Ishwaran H. and Kogalur U.B. randomForestSRC: Random Forests for
Survival, Regression and Classification. R package version \>= 3.4.0.
<https://cran.r-project.org/package=randomForestSRC>

## See also

[`gg_vimp`](https://ehrlinger.github.io/ggRandomForests/reference/gg_vimp.md)

## Examples

``` r
## ------------------------------------------------------------
## classification example
## ------------------------------------------------------------
## -------- iris data
rfsrc_iris <- rfsrc(Species ~ ., data = iris)
gg_dta <- gg_vimp(rfsrc_iris)
#> Warning: rfsrc object does not contain VIMP information. Calculating...
plot(gg_dta)


## ------------------------------------------------------------
## regression example
## ------------------------------------------------------------
## -------- air quality data
rfsrc_airq <- rfsrc(Ozone ~ ., airquality)
gg_dta <- gg_vimp(rfsrc_airq)
#> Warning: rfsrc object does not contain VIMP information. Calculating...
plot(gg_dta)


```
