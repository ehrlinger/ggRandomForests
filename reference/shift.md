# Lead / lag shift for numeric vectors

Lead / lag shift for numeric vectors

## Usage

``` r
shift(x, shift_by = 1)
```

## Arguments

- x:

  a numeric vector of values

- shift_by:

  an integer of length 1, giving the number of positions to lead
  (positive) or lag (negative) by

## Details

Lead and lag are useful for comparing values offset by a constant (e.g.
the previous or next value).

Taken from:
http://ctszkin.com/2012/03/11/generating-a-laglead-variables/

This function allows removal of the dplyr::lead dependency.

## Examples

``` r
d <- data.frame(x = 1:15)
# generate lead variable
d$df_lead2 <- ggRandomForests:::shift(d$x, 2)
# generate lag variable
d$df_lag2 <- ggRandomForests:::shift(d$x, -2)
```
