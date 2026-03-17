# Recover original variable names from varpro one-hot encoded feature names

`varpro` one-hot encodes factor variables, appending a numeric suffix
for each level (e.g., `sex` becomes `sex0` and `sex1`). This function
strips those suffixes iteratively until every name in `varpro_names` can
be matched back to a column in `dataset`.

## Usage

``` r
varpro_feature_names(varpro_names, dataset)
```

## Arguments

- varpro_names:

  character vector of names as output by varpro (may include one-hot
  encoded suffixed names such as `"sex0"`, `"sex1"`)

- dataset:

  the original data frame passed to varpro, used to look up valid column
  names

## Value

character vector of unique original variable names (no suffixes)

## See also

[`gg_partialpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partialpro.md)

## Examples

``` r
## ------------------------------------------------------------------
## Simple case: one continuous variable + one binary factor
## ------------------------------------------------------------------
ds <- data.frame(age = c(25, 30, 45), sex = c("M", "F", "M"))

# varpro one-hot encodes 'sex' into 'sex0' and 'sex1'
varpro_names <- c("age", "sex0", "sex1")
varpro_feature_names(varpro_names, ds)
#> [1] "age" "sex"
# Returns: c("age", "sex")

## ------------------------------------------------------------------
## Multi-level factor: three-level 'group' variable
## ------------------------------------------------------------------
ds2 <- data.frame(score = 1:6,
                  group = factor(rep(c("A", "B", "C"), 2)))

# varpro appends 0/1/2 for each level
vn2 <- c("score", "group0", "group1", "group2")
varpro_feature_names(vn2, ds2)
#> [1] "score" "group"
# Returns: c("score", "group")

## ------------------------------------------------------------------
## Already-clean names pass through unchanged
## ------------------------------------------------------------------
ds3 <- data.frame(x = 1:5, y = 1:5)
varpro_feature_names(c("x", "y"), ds3)
#> [1] "x" "y"
# Returns: c("x", "y")
```
