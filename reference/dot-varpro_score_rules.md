# Re-extract release rules and score them as importance() does

The rule-sampling half of
[`varPro::importance()`](https://www.randomforestsrc.org/reference/importance.html).
With `local.std = TRUE` each usable rule (out-of-bag and complement
cases both present) gets its local importance and only those rules are
kept; with `FALSE` the strength array's own importance is used,
unfiltered. Returns the usable rules and a copy of `object` whose
`$results` holds the scored rules, ready for
`importance.varpro.workhorse()`.

## Usage

``` r
.varpro_score_rules(
  object,
  max.tree,
  max.rules.tree,
  seed = NULL,
  local.std = TRUE
)
```
