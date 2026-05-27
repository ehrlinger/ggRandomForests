# Per-variable lasso-β importance from a varPro fit

Tidy wrapper around
[`varPro::beta.varpro()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
for the regression family. Aggregates the per-rule lasso coefficient (β̂)
by variable into `mean(|β̂|)` and flags variables above a scalar cutoff.
Optional `beta_fit` argument lets callers compute the expensive
`beta.varpro()` step once and reuse the result.

## Usage

``` r
gg_beta_varpro(object, ..., cutoff = NULL, beta_fit = NULL)
```

## Arguments

- object:

  A `varpro` fit from
  [`varPro::varpro()`](https://www.randomforestsrc.org/reference/varpro.html)
  (regression family).

- ...:

  Forwarded to
  [`varPro::beta.varpro()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
  when `beta_fit = NULL`; ignored otherwise (with a warning). Documented
  forwardables: `use.cv`, `use.1se`, `nfolds`, `maxit`, `thresh`,
  `max.rules.tree`, `max.tree`.

- cutoff:

  Selection threshold on `beta_mean`. `NULL` (default) →
  `mean(beta_mean)` across released variables. Numeric scalar otherwise.

- beta_fit:

  Optional pre-computed
  [`varPro::beta.varpro()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
  result for the same `object`. `NULL` (default) → the wrapper runs
  `beta.varpro()` itself. When supplied, must be a `varpro`-class object
  whose `$results` has columns `tree / branch / variable / n.oob / imp`.

## Value

A `data.frame` of class `c("gg_beta_varpro", "data.frame")`, one row per
released variable, sorted by `beta_mean` descending.

## Note

Classification, multivariate regression (`regr+`), and survival families
are out of scope for this release. The non-regression path errors with a
message naming Phase 4d as the tracker (see NEWS).

## What this is doing

For each rule (a tree-branch pair) in the forest,
[`varPro::beta.varpro()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
fits a one-predictor lasso regression of the response on the released
variable's values, restricted to the OOB observations inside the rule's
region. The wrapper aggregates those per-rule coefficients into one
number per variable.

## What `imp` actually is (pedantic, because the column name is misleading)

The `imp` column on `beta.varpro()`'s `$results` is **not** a
variable-importance score in the conventional sense. It is a regularised
regression coefficient. Specifically:

- Per rule, `glmnet` fits a one-predictor lasso of the response on the
  released variable inside the rule's OOB region. `use.cv = TRUE`
  selects λ by 10-fold CV (default `nfolds = 10`); `use.1se = TRUE`
  (default) picks `lambda.1se`. `use.cv = FALSE` uses the full λ path.

- `imp` is the **fitted coefficient β̂** at the chosen λ. **Sign is
  real** (direction of local association). **Magnitude depends on the
  predictor's units** (raw `x`, no standardisation); a predictor in
  millimetres has a smaller \|β̂\| than the same predictor in metres.

- Lasso shrinkage can drive β̂ to **exactly zero**. Those zeros are data,
  not missingness, and are kept in the aggregation. Convergence failures
  land as `NA_real_` and are dropped.

- The per-variable aggregate is `beta_mean = mean(|β̂|)` across the rules
  where this variable was released. It is **not** a permutation
  importance, **not** a split-strength importance, and **not** directly
  comparable on the same numeric axis to
  [`gg_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md)'s
  z-scores. Disagreement with `gg_varpro` is often diagnostic, not a
  bug.

In code form:
`imp_r = β̂_glmnet(y | x_v restricted to rule r, λ chosen by use.cv / use.1se)`.

## What's in the output

One row per released variable. Columns:

- `variable`: predictor name.

- `beta_mean`: mean of `|β̂|` across that variable's rules.

- `n_rules`: count of rules contributing (zero-β rules included; only
  `NA` failures excluded).

- `selected`: `beta_mean >= cutoff`.

Provenance attribute carries `source`, `family`, `ntree`, `cutoff`,
`cutoff_default`, `use.cv`, `n_rules_total`, `n_rules_nonzero`,
`precomputed`, and `xvar.names`.

## What you use this for

Picking variables when local effects matter more than aggregate
split-strength contribution. Compare side-by-side with
[`gg_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md)
— a variable that scores high here but low in `gg_varpro` is one whose
local linear effect inside many rules is real even though its
release-rule contrast is modest.

## Caching

`beta.varpro()` is the expensive call (per-rule `glmnet` / `cv.glmnet`,
often minutes on real data). Compute it once and reuse:

    v <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 200)
    b <- varPro::beta.varpro(v, use.cv = TRUE)        # expensive, once
    gg_a <- gg_beta_varpro(v, beta_fit = b)            # cheap
    gg_b <- gg_beta_varpro(v, beta_fit = b, cutoff = 0.5)

Provenance carries `precomputed = TRUE` when `beta_fit` was supplied.

## Reproducibility

Byte-for-byte agreement between cached (`beta_fit = b`) and uncached
(`beta_fit = NULL`) outputs requires that `b` was computed by
`beta.varpro(object, ...)` on the same `object` —
[`set.seed()`](https://rdrr.io/r/base/Random.html) alone is not
sufficient, because `beta.varpro`'s internal `cv.glmnet` fits can pick
slightly different folds across separate calls. Reuse `beta_fit` when
reproducibility matters.

## See also

[`gg_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md),
[`plot.gg_beta_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_beta_varpro.md),
[`varPro::beta.varpro()`](https://www.randomforestsrc.org/reference/utilities_internal.html).

## Examples

``` r
# \donttest{
if (requireNamespace("varPro", quietly = TRUE)) {
  set.seed(1)
  v <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)
  b <- varPro::beta.varpro(v)
  gg <- gg_beta_varpro(v, beta_fit = b)
  plot(gg)
}

# }
```
