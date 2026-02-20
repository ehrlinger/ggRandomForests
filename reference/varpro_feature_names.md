# varpro one hot encodes features, so we need to get the "raw" original variable names. This loops through the variable names not in the original dataset, and cuts one character off the end until we find the variable name in the original data.

varpro one hot encodes features, so we need to get the "raw" original
variable names. This loops through the variable names not in the
original dataset, and cuts one character off the end until we find the
variable name in the original data.

## Usage

``` r
varpro_feature_names(varpro_names, dataset)
```

## Arguments

- varpro_names:

  vector of names output from varpro analysis

- dataset:

  the dataset used for varpro input.
