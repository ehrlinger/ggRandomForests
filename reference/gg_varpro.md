# Variable importance data from a varPro model

Pulls the per-tree importance scores out of a fitted `varpro` object and
summarizes them into a data structure the plot method can draw as a
boxplot. The box hinges are the 15th and 85th percentiles and the
whiskers run to the 5th and 95th, not the usual Tukey 1.5 IQR whiskers.
For a classification forest you can also keep the class-conditional
importances.

## Usage

``` r
gg_varpro(
  object,
  local.std = TRUE,
  cutoff = 0.79,
  faithful = FALSE,
  conditional = FALSE,
  nvar = NULL,
  trim = 0.1,
  max.tree = object$max.tree,
  max.rules.tree = object$max.rules.tree,
  ...
)
```

## Arguments

- object:

  A fitted `varpro` object (required).

- local.std:

  Logical; default `TRUE`. Passed to varPro's importance aggregation,
  with the same meaning as in
  [`varPro::importance()`](https://www.randomforestsrc.org/reference/importance.html):
  `TRUE` re-samples the release rules, scores them by local importance
  and reports z as their winsorised mean, so the boxes are on z's scale.
  `FALSE` uses the fit-time rules and standardises z by the winsorised
  standard deviation; the boxes are then on the raw importance scale,
  which is what `type = "raw"` in `plot.gg_varpro` needs. With `TRUE`
  the result depends on the random number stream, so call
  [`set.seed()`](https://rdrr.io/r/base/Random.html) first for a
  reproducible z.

- cutoff:

  Numeric; the z-score above which a variable is treated as selected.
  Default `0.79`. A variable with aggregate z above the cutoff is
  flagged `selected = TRUE` in `$imp`.

- faithful:

  Logical; default `FALSE`. When `TRUE`, `$imp.tree` is kept so
  `plot.gg_varpro` can scatter the per-tree points over the box.

- conditional:

  Logical; default `FALSE`. When `TRUE`, and only for a classification
  forest, the `$conditional.z` matrix is extracted and stored as
  `$conditional`.

- nvar:

  Integer; keep only the top `nvar` variables, ranked by median per-tree
  z, after the cutoff filter has been applied. `NULL` keeps all of them.

- trim:

  Winsorising fraction used for z, as in
  [`varPro::importance()`](https://www.randomforestsrc.org/reference/importance.html);
  default `0.1`. A variable is `tested` when its coverage is at least
  `trim`.

- max.tree, max.rules.tree:

  Number of trees, and release rules per tree, to score. Default to the
  values stored in the fit. Supplying either one re-samples the rules
  even with `local.std = FALSE`, as
  [`varPro::importance()`](https://www.randomforestsrc.org/reference/importance.html)
  does. The boxes, z and coverage are always computed from the same
  rules.

- ...:

  Not used. Earlier versions passed it to
  [`varPro::importance()`](https://www.randomforestsrc.org/reference/importance.html);
  `trim`, `max.tree` and `max.rules.tree` are now arguments, and
  anything else is an error.

## Value

A named list of class `"gg_varpro"` with elements:

- `$imp`:

  Summary data frame: `variable` (factor whose levels run least- to
  most-important by median per-tree z, so the most-important variable
  sits at the top of the plot after `coord_flip`), `z` (aggregate
  z-score, `NA` when it is undefined), `selected` (logical,
  `z > cutoff`), `coverage` (share of the scored trees in which a
  release rule for the variable returned an importance value) and
  `tested` (`coverage >= trim`).

- `$imp.tree`:

  `NULL` when `faithful = FALSE`; otherwise an ntree x p matrix of
  per-tree importance values.

- `$stats`:

  Per-variable summary: `variable`, `median`, `q05`, `q15`, `q85`, `q95`
  of the per-tree values (on z's scale when `local.std = TRUE`, raw when
  `FALSE`), `mean` (their plain mean), and `coverage` and `tested` as in
  `$imp`.

- `$conditional`:

  `NULL` when `conditional = FALSE`; otherwise a data frame with columns
  `variable`, `class`, `z` (one row per variable x class combination).
  `class` is a factor of the outcome's own level names, also for a
  binary outcome, which varPro recodes to 0/1 internally.

A `"provenance"` attribute carries `family`, `local.std`, `cutoff`,
`faithful`, `conditional`, `xvar.names`, `n`, `trim`, `max.tree` and
`max.rules.tree`.

## What varpro is doing

Permutation importance asks "what happens to OOB accuracy when I
scramble this variable?" That works, but it leans on artificial data
(the permuted column) and the answer can be unstable when variables are
correlated. The varpro framework (Lu and Ishwaran, 2024) replaces
permutation with *release rules*. The forest is grown with guided
splitting; from a subset of trees varpro samples a collection of
decision-rule branches; for each variable it then compares the response
inside the rule's region to the response after the rule's constraint on
that variable is "released". The size of that change, aggregated over
many rules and trees, is the variable's importance. No synthetic
covariates, no permutation: the contrast is between two real subsets of
the data.

Because varpro builds importance from rules sampled over trees, every
tree contributes its own importance value for each variable. Those are
the per-tree scores we summarize here, and the aggregate z comes from
the same rules on the same trees, so the boxes and z always agree.
[`varPro::importance()`](https://www.randomforestsrc.org/reference/importance.html)
takes a winsorised mean over trees (trimmed by `trim`), counting a tree
with no usable release rule for the variable as 0. With
`local.std = TRUE` (the default, as in varPro) the release rules are
re-sampled and scored by their local importance, the standard deviation
is fixed at 1, and z is that winsorised mean, so the per-tree values
already sit on z's scale. With `local.std = FALSE` the fit-time rules
are used and z is the winsorised mean divided by the winsorised standard
deviation. `cutoff = 0.79` is varpro's default selection threshold, but
the scale of z depends on how many variables share each tree, so the
same cutoff does not mean the same thing across settings.

A variable scored in fewer than about `trim` of the trees (a tree scores
it when one of its release rules for the variable returns an importance
value) is *untested*: its z is 0 (or undefined with `local.std = FALSE`)
because every non-zero tree is winsorised away, not because it was
tested and found unimportant. `coverage` and `tested` in the output say
which variables those are, and the plot draws them as open boxes.
[`varpro_coverage`](https://ehrlinger.github.io/ggRandomForests/reference/varpro_coverage.md)
counts coverage the same way and adds a per-variable roll-up and a
noise-based cutoff.

For a classification forest, varpro also returns a class-conditional z
table: the same importance computed restricting attention to rules
relevant to each class. `conditional = TRUE` keeps that table so the
plot method can show which variables matter for which class rather than
only in aggregate.

## What's in the output

`$imp` is the one-row-per-variable summary: aggregate z from varPro's
importance aggregation, a `selected` flag for `z > cutoff`, and the
variable's `coverage` and `tested` flag. `$stats` holds the box
quantiles (5/15/50/85/95 percentiles, plus the raw mean) computed from
the per-tree matrix; these are what the boxplot draws. `$imp.tree` is
the per-tree matrix itself, kept only when `faithful = TRUE` so the plot
method can scatter individual tree values over the box. `$conditional`
is the tidy class x variable z table, present only when
`conditional = TRUE` and the family is classification.

## What you use this for

- rank candidate variables by importance and pick a working set above
  varpro's z cutoff;

- see, via the boxplot's spread and the per-tree points
  (`faithful = TRUE`), how stable each variable's importance is across
  trees: a high median with a wide box is a different story from a high
  median with a tight box;

- for a classification forest, ask which variables drive which class
  (`conditional = TRUE`) rather than just which variables drive the
  model overall.

The z-score is a standardized ranking statistic, not a p-value or a
probability. Two variables with the same z are "similarly important by
this method", not "equally likely to be true signal". For a data-driven
cutoff rather than the 0.79 default, see
[`varPro::cv.varpro`](https://www.randomforestsrc.org/reference/cv.varpro.html).

## References

Lu, M. and Ishwaran, H. (2024). Model-independent variable selection via
the rule-based variable priority framework. *arXiv preprint*
[arXiv:2409.09003](https://arxiv.org/abs/2409.09003).

## See also

[`plot.gg_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_varpro.md),
[`varpro_coverage`](https://ehrlinger.github.io/ggRandomForests/reference/varpro_coverage.md),
[`gg_vimp`](https://ehrlinger.github.io/ggRandomForests/reference/gg_vimp.md)

## Examples

``` r
# \donttest{
set.seed(42)
vp <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)
gg <- gg_varpro(vp)
print(gg)
#> <gg_varpro>  family: regr  |  n: 32  |  family: regr  |  cutoff: 0.79  |  faithful: FALSE
#>   2 of 4 variables selected (z > 0.79)
plot(gg)

# }
```
