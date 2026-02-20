# r_data_types infers correct data classes for each column in a data.frame

Read an input data set and infer the correct columnar r data types.
Includes numeric, logical or factor type of the data columns

## Usage

``` r
r_data_types(dataset, factor_size = 10)
```

## Arguments

- dataset:

  a dataframe to modify

- factor_size:

  numeric features may only have a small number of unique values. Where
  should we turn these into factors.

## Value

A `data.frame` with datatypes logically encoded.

## Examples

``` r
## ------------------------------------------------------------
## -------- pbc data
# We need to create this dataset
data(pbc, package = "randomForestSRC" )

# Show data types for each column
sapply(pbc,class)
#>        days      status   treatment         age         sex     ascites 
#>   "integer"   "integer"   "integer"   "integer"   "integer"   "integer" 
#>     hepatom     spiders       edema        bili        chol     albumin 
#>   "integer"   "integer"   "numeric"   "numeric"   "integer"   "numeric" 
#>      copper         alk        sgot        trig    platelet prothrombin 
#>   "integer"   "numeric"   "numeric"   "integer"   "integer"   "numeric" 
#>       stage 
#>   "integer" 

# Correct types
pbc <- r_data_types(pbc)
sapply(pbc,class)
#>        days      status   treatment         age         sex     ascites 
#>   "integer"   "logical"   "logical"   "integer"   "logical"   "logical" 
#>     hepatom     spiders       edema        bili        chol     albumin 
#>   "logical"   "logical"    "factor"   "numeric"   "integer"   "numeric" 
#>      copper         alk        sgot        trig    platelet prothrombin 
#>   "integer"   "numeric"   "numeric"   "integer"   "integer"   "numeric" 
#>       stage 
#>    "factor" 
```
