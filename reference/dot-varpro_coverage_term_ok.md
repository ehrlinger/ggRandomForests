# Which terms were scored often enough to be tested

A term is tested when rules scored it in at least `trim` of the `trees`
and in at least one tree, so that `trim = 0` does not make an unscored
term tested. Returns a logical vector named by term.

## Usage

``` r
.varpro_coverage_term_ok(rule_term, tree_id, terms, trees, trim)
```
