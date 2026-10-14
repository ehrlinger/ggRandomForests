# Original variable of each encoded term, from varPro's stored map

varPro stores the term-to-variable map as the `"xvar.map"` attribute of
`object$x`. Suffix stripping cannot be trusted: a factor `g` with a
level encoded `ga.1` and a numeric `ga` collide.

## Usage

``` r
.varpro_coverage_map(object, terms)
```
