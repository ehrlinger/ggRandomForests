# Split partial lots into continuous or categorical datasets

Split partial lots into continuous or categorical datasets

## Usage

``` r
gg_partialpro(part_dta, nvars = NULL, cat_limit = 10, model = NULL)
```

## Arguments

- part_dta:

  partial plot data from `varpro::partialpro`

- nvars:

  how many of the partial plot variables to calculate

- cat_limit:

  Categorical features are build when there are fewer than cat_limit
  unique features.

- model:

  a label name applied to all features. Useful when combining multiple
  partial plot objects in figures.
