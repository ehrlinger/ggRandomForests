# Split partial lots into continuous or categorical datasets

gg_partial_rfsrc uses the `rfsrc::partial.rfsrc` to generate the partial
plot data internally. So you provide the `rfsrc::rfsrc` model, and the
xvar.names to generate the data.

## Usage

``` r
gg_partial_rfsrc(
  rf_model,
  xvar.names = NULL,
  xvar2.name = NULL,
  newx = NULL,
  cat_limit = 10
)
```

## Arguments

- rf_model:

  `rfsrc::rfsrc` model

- xvar.names:

  list(\<str\>) Which variables to calculate partial plots

- xvar2.name:

  \<str\> a single grouping feature that is in the newx dataset

- newx:

  a `data.frame` containing data to use for the partial plots

- cat_limit:

  Categorical features are build when there are fewer than cat_limit
  unique features.

## Examples

``` r
## ------------------------------------------------------------
##
## regression
##
## ------------------------------------------------------------

airq.obj <- rfsrc(Ozone ~ ., data = airquality)

## partial effect for wind
prt_dta <- gg_partial_rfsrc(airq.obj,
                       xvar.names = c("Wind"))
```
