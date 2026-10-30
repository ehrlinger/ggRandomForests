# How well a varpro fit tested each variable

Reports, for every term varpro could test, how many of the forest's
trees scored it with a release rule, next to its importance z. Use it to
tell a variable that was tested and found unimportant from one that was
barely tested at all.

## Usage

``` r
varpro_coverage(
  object,
  by = c("term", "variable"),
  trim = 0.1,
  max.tree = object$rf$ntree,
  max.rules.tree = object$max.rules.tree,
  noise = NULL,
  kind_values = 5L,
  seed = NULL,
  max_untested = NULL
)
```

## Arguments

- object:

  A fitted `varpro` object.

- by:

  `"term"` (one row per term in the fit's data, factor levels
  separately) or `"variable"` (one-hot levels rolled up to their
  original variable: tested in a tree when any level is, z the largest
  level z).

- trim:

  Coverage below which a term counts as untested, and the winsorisation
  trim used for z. Defaults to 0.1, as in
  [`varPro::importance()`](https://www.randomforestsrc.org/reference/importance.html):
  the share of trees below which z is 0.

- max.tree:

  Number of trees to score. Defaults to every tree, rather than varPro's
  sample of 150, so that coverage rests on all the evidence. Coverage
  and z always come from the same extracted rules.

- max.rules.tree:

  Maximum release rules sampled per tree; defaults to the value stored
  in `object`. Sampling only happens in trees with more branches than
  this.

- noise:

  `NULL`, or a regular expression matching the names of noise variables
  (for example `"^noise"`).

- kind_values:

  A numeric variable with at most this many distinct values counts as
  categorical for the noise comparison.

- seed:

  Seed passed to
  [`varPro::varpro.strength()`](https://www.randomforestsrc.org/reference/varpro.strength.html),
  which samples the release rules; with a seed (or a
  [`set.seed()`](https://rdrr.io/r/base/Random.html) call first) the
  table is reproducible.

- max_untested:

  `NULL` (the default: report only), or the largest share of real
  variables, from 0 to 1, that may be untested. Rows are counted as `by`
  lists them and noise rows are left out. A row counts as tested only
  when some term of it (for a factor, some single level) was scored in
  at least `trim` of the trees and in at least one, so ineligible rows
  always count as untested. When the share is larger the call stops with
  an error, so that a screen that tested too little cannot be read as
  one that found no effect.

## Value

A data frame of class `c("varpro_coverage", "data.frame")`, one row per
term (or variable), most-covered first, with columns `term` (term rows
only), `variable`, `n_terms` (variable rows only), `eligible` (varpro
built rules for it; `FALSE` when the split weights or `nvar` cut it
first), `n_trees` (trees with a rule that scored it), `coverage`
(`n_trees` over the trees z is averaged over), `n_rules` (rules that
scored it), `z` (computed as
[`varPro::importance()`](https://www.randomforestsrc.org/reference/importance.html)
computes it, with its default `local.std = TRUE`, from the same rules;
`NA` when never returned) and `tested` (`coverage >= trim`). With
`noise` set it adds `noise` (logical), `kind`, `noise_cut`, `noise_ref`
(the kind of the noise that set `noise_cut`: the row's own kind, the
other kind when its own has no tested noise, or `NA` when no noise was
tested) and `selected` (a tested real row, scored in at least one tree,
whose z beats `noise_cut`). Attributes `by`, `trees` (the coverage
denominator), `trim` and `max.rules.tree` record the settings.

## Why coverage matters

[`varPro::importance()`](https://www.randomforestsrc.org/reference/importance.html)
scores a term by a trimmed (winsorised) mean of its importance in each
tree, and a tree without a usable release rule for the term counts as
zero. A term tested in fewer than about `trim` of the trees therefore
has every non-zero value trimmed away and comes back with z exactly 0:
it is *untested*, not unimportant. A term varpro does not return at all
was never tested, either because it was cut before rule-building (split
weights and `nvar`) or because no rule had cases on both sides of its
release. Neither is evidence of no effect.

A rule is usable when its out-of-bag region and its released complement
both hold cases; it scores the term when its local importance also comes
back as a value. Coverage is the share of scored trees with at least one
rule that scored the term, out of the trees z is averaged over (those
holding any usable rule). It counts exactly what
[`gg_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md)
counts, so on the same rules the two agree on `coverage` and `tested`.

## Noise reference

varpro's default cutoff (z \> 0.79) does not carry from one fit to
another with the default `local.std = TRUE`: z is then an unstandardised
mean, and it shrinks as more terms share each tree. If the data include
permuted copies of real columns (noise by construction), name them with
a common prefix and pass a pattern as `noise`. Each real row is then
compared with the largest z among *tested* noise rows of the same kind
(categorical or continuous); an untested noise row has z = 0 by
construction and sets no bar. Here tested also means scored in at least
one tree, so `trim = 0` does not make unscored noise a bar at 0. A kind
with no tested noise falls back to the largest tested-noise z overall;
`noise_ref` records it and a warning says so. ggRandomForests does not
add the noise columns; do that before fitting.

## The first-pass filter

Before it builds any rules,
[`varPro::varpro()`](https://www.randomforestsrc.org/reference/varpro.html)
screens the terms with split weights. With its default
`split.weight = TRUE` it weights every term (by lasso, variable
importance or tree usage), drops any term whose weight is 0, and keeps
only the `nvar` heaviest (30 by default). Every other term is
ineligible: no rule is ever built for it, so it is untested however
strong its effect. On wide data most terms can be cut here. Fit with
`split.weight = FALSE` to make every term eligible; `nvar` then has no
effect. Custom weights passed as `split.weight.custom` take precedence
over that switch: terms they weight 0 or leave out, and terms beyond
`nvar`, are still cut, so drop the custom weights too. varPro's hidden
`split.weight.tolerance` only floors the weights of the terms that
survive and does not change which terms are cut.

## See also

[`gg_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md),
[`varpro_feature_names`](https://ehrlinger.github.io/ggRandomForests/reference/varpro_feature_names.md)

## Examples

``` r
# \donttest{
if (requireNamespace("varPro", quietly = TRUE)) {
  set.seed(20261014)
  d <- mtcars
  d$noise_wt <- sample(d$wt)        # permuted copy: noise by construction
  vp <- varPro::varpro(mpg ~ ., data = d, ntree = 50)
  set.seed(20261014)
  varpro_coverage(vp, noise = "^noise")
}
#> Warning: no noise variable was tested (scored in at least trim of the trees, and in at least one), so the noise check is empty: noise_cut is 0 and every tested row is selected.
#>        term variable eligible n_trees coverage n_rules         z tested noise
#> 1        wt       wt     TRUE      31     0.62      67 1.6556518   TRUE FALSE
#> 2       cyl      cyl     TRUE      20     0.40      37 1.3485590   TRUE FALSE
#> 3        hp       hp     TRUE      19     0.38      47 0.9587131   TRUE FALSE
#> 4      disp     disp     TRUE       2     0.04       4 0.0000000  FALSE FALSE
#> 5      drat     drat     TRUE       0     0.00       0        NA  FALSE FALSE
#> 6      qsec     qsec     TRUE       0     0.00       0        NA  FALSE FALSE
#> 7        vs       vs     TRUE       0     0.00       0        NA  FALSE FALSE
#> 8        am       am    FALSE       0     0.00       0        NA  FALSE FALSE
#> 9      gear     gear     TRUE       0     0.00       0        NA  FALSE FALSE
#> 10     carb     carb     TRUE       0     0.00       0        NA  FALSE FALSE
#> 11 noise_wt noise_wt     TRUE       0     0.00       0        NA  FALSE  TRUE
#>           kind noise_cut noise_ref selected
#> 1   continuous         0      <NA>     TRUE
#> 2  categorical         0      <NA>     TRUE
#> 3   continuous         0      <NA>     TRUE
#> 4   continuous         0      <NA>    FALSE
#> 5   continuous         0      <NA>    FALSE
#> 6   continuous         0      <NA>    FALSE
#> 7  categorical         0      <NA>    FALSE
#> 8  categorical         0      <NA>    FALSE
#> 9  categorical         0      <NA>    FALSE
#> 10  continuous         0      <NA>    FALSE
#> 11  continuous         0      <NA>    FALSE
# }
```
