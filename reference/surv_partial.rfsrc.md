# Survival partial dependence data for one or more predictors

**Deprecated.** Use
[`gg_partial_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
instead, which returns a classed `gg_partial_rfsrc` object with a
dedicated [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
method.

## Usage

``` r
surv_partial.rfsrc(rforest, var_list, npts = 25, partial.type = "surv")
```

## Arguments

- rforest:

  A fitted
  [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html)
  survival or competing-risk forest object.

- var_list:

  Character vector of predictor names for which partial dependence
  should be computed. Each must appear in `rforest$xvar.names`.

- npts:

  Integer; the number of predictor grid points to evaluate (default 25).
  Evenly-spaced unique values are sampled from each predictor.

- partial.type:

  The prediction type to return. For survival forests one of `"surv"`
  (default), `"mort"`, or `"chf"`. For competing risk forests one of
  `"years.lost"`, `"cif"`, or `"chf"`. See
  [`partial.rfsrc`](https://www.randomforestsrc.org//reference/partial.rfsrc.html)
  for full details.

## Value

A named list with one element per variable in `var_list`. Each element
is itself a list with:

- name:

  The predictor variable name (character).

- dta:

  The raw output of
  [`get.partial.plot.data`](https://www.randomforestsrc.org//reference/partial.rfsrc.html),
  a list containing at minimum `x` (predictor values) and `yhat`
  (partial predictions), and for survival/competing risk,
  `partial.time`.

## Details

Computes partial dependence curves for a survival or competing-risk
[`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html) forest
by calling
[`partial.rfsrc`](https://www.randomforestsrc.org//reference/partial.rfsrc.html)
at `npts` evenly-spaced unique values of each predictor across all
stored event times.

## See also

[`gg_partial_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md),
[`partial.rfsrc`](https://www.randomforestsrc.org//reference/partial.rfsrc.html),
[`get.partial.plot.data`](https://www.randomforestsrc.org//reference/partial.rfsrc.html)

## Examples

``` r
## ------------------------------------------------------------
## survival
## ------------------------------------------------------------

data(veteran, package = "randomForestSRC")
v.obj <- randomForestSRC::rfsrc(Surv(time,status)~.,
  veteran, nsplit = 10, ntree = 100)

spart <- surv_partial.rfsrc(v.obj, var_list="age", partial.type = "mort")
#> Warning: 'surv_partial.rfsrc' is deprecated and will be removed in a future release.
#> Use 'gg_partial_rfsrc()' instead, which returns a classed object with a
#> dedicated plot() method.
#> partial plot for: age

## partial effect of age on mortality
partial.obj <- partial(v.obj,
                       partial.type = "mort",
                       partial.xvar = "age",
                       partial.values = v.obj$xvar$age,
                       partial.time = v.obj$time.interest)
pdta <- get.partial.plot.data(partial.obj)

plot(lowess(pdta$x, pdta$yhat, f = 1/3),
     type = "l", xlab = "age", ylab = "adjusted mortality")


## example where x is discrete - partial effect of age on mortality
## we use the granule=TRUE option
partial.obj <- partial(v.obj,
                       partial.type = "mort",
                       partial.xvar = "trt",
                       partial.values = v.obj$xvar$trt,
                       partial.time = v.obj$time.interest)
pdta <- get.partial.plot.data(partial.obj, granule = TRUE)
boxplot(pdta$yhat ~ pdta$x, xlab = "treatment", ylab = "partial effect")



## partial effects of karnofsky score on survival
karno <- quantile(v.obj$xvar$karno)
partial.obj <- partial(v.obj,
                       partial.type = "surv",
                       partial.xvar = "karno",
                       partial.values = karno,
                       partial.time = v.obj$time.interest)
pdta <- get.partial.plot.data(partial.obj)

matplot(pdta$partial.time, t(pdta$yhat), type = "l", lty = 1,
        xlab = "time", ylab = "karnofsky adjusted survival")
legend("topright", legend = paste0("karnofsky = ", karno), fill = 1:5)



## ------------------------------------------------------------
## competing risk
## ------------------------------------------------------------

data(follic, package = "randomForestSRC")
follic.obj <- rfsrc(Surv(time, status) ~ ., follic, nsplit = 3, ntree = 100)

## partial effect of age on years lost
partial.obj <- partial(follic.obj,
                       partial.type = "years.lost",
                       partial.xvar = "age",
                       partial.values = follic.obj$xvar$age,
                       partial.time = follic.obj$time.interest)
pdta1 <- get.partial.plot.data(partial.obj, target = 1)
pdta2 <- get.partial.plot.data(partial.obj, target = 2)

# Save and restore the user's graphical parameters per CRAN policy.
oldpar <- par(no.readonly = TRUE)
on.exit(par(oldpar))
par(mfrow = c(2, 2))
plot(lowess(pdta1$x, pdta1$yhat),
     type = "l", xlab = "age", ylab = "adjusted years lost relapse")
plot(lowess(pdta2$x, pdta2$yhat),
     type = "l", xlab = "age", ylab = "adjusted years lost death")

## partial effect of age on cif
partial.obj <- partial(follic.obj,
                       partial.type = "cif",
                       partial.xvar = "age",
                       partial.values = quantile(follic.obj$xvar$age),
                       partial.time = follic.obj$time.interest)
pdta1 <- get.partial.plot.data(partial.obj, target = 1)
pdta2 <- get.partial.plot.data(partial.obj, target = 2)

matplot(pdta1$partial.time, t(pdta1$yhat), type = "l", lty = 1,
        xlab = "time", ylab = "age adjusted cif for relapse")
matplot(pdta2$partial.time, t(pdta2$yhat), type = "l", lty = 1,
        xlab = "time", ylab = "age adjusted cif for death")

```
