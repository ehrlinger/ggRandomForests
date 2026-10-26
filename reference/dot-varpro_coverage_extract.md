# One release-rule extraction, scored as importance() scores it

Mirrors the default (`local.std = TRUE`) path of
[`varPro::importance()`](https://luminwin.github.io/reference/importance.html):
rules from `varpro.strength()` with memberships, local importance for
each usable rule (out-of-bag and complement cases both present), then
varPro's own aggregation. Returns the usable rules and the importance
table built from exactly those rules.

## Usage

``` r
.varpro_coverage_extract(object, max.tree, max.rules.tree, trim, seed)
```
