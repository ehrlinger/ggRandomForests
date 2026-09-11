# Plot a [`gg_partial_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md) object

Draws the partial dependence curves from the list that
[`gg_partial_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)
returns. Continuous predictors get overlaid line curves, one per effect
type; categorical predictors get side-by-side boxplots. Survival path-C
objects (the ones you get when `scale %in% c("surv","chf")` was passed
to the extractor) are handed off to
[`plot.gg_partial_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_rfsrc.md)
for drawing.

## Usage

``` r
# S3 method for class 'gg_partialpro'
plot(x, type = c("parametric", "nonparametric", "causal"), ...)

# S3 method for class 'gg_partial_varpro'
plot(x, type = c("parametric", "nonparametric", "causal"), ...)
```

## Arguments

- x:

  A
  [`gg_partial_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)
  object.

- type:

  Character vector; one or more of `"parametric"`, `"nonparametric"`,
  `"causal"`. Defaults to all three. Ignored for path-C objects.

- ...:

  Unused for path-A objects; forwarded to `plot.gg_partial_rfsrc` for
  path-C objects.

## Value

A `ggplot` (or `patchwork`) object.

## Details

**Ensemble mortality (scale = "mortality"):** when the provenance scale
is `"mortality"`, the y-axis is labeled *"Ensemble mortality (expected
events)"*. The wording is deliberate: this is an **unbounded
relative-risk score**, not a survival probability and not \\1 - S(t)\\
(Ishwaran, Kogalur, Blackstone & Lauer, 2008 <doi:10.1214/08-AOAS169>).

## Reading the partial dependence

For a continuous variable the x-axis is the variable's grid of values
and the y-axis is the partial prediction; each of the three effect types
(`parametric`, `nonparametric`, `causal`) is drawn as its own line. The
shape of the line is the story: a clear slope says the model uses the
variable, a flat line says it essentially does not, and a U-shape or a
threshold says the effect is nonlinear in a way a single coefficient
would miss. For a categorical variable the picture is a boxplot per
level; here the eye is looking at level-to-level shifts in the center of
each box.

Where the three effect types track each other, the parametric story is a
fair summary of what the forest is doing. Where they fan apart
(typically the parametric curve smoother than the nonparametric, or the
causal curve flatter than either) the variable is one to inspect more
carefully before reading a single effect off the plot.

## What this tells you

Use these curves to describe how the model uses each variable, not to
claim how the world works. They are a window into the fitted
relationship; they do not by themselves establish that intervening on
the variable would move the outcome. For survival path-C
(`scale = "chf"`), the y-axis is on the cumulative-hazard scale.

## Reading a probability curve (scale = "prob")

The y-axis is \\P(Y = \mathrm{target})\\, the model's predicted
probability of the target class as the focal variable varies (others
held at their UVT-plausible average). `"odds"` and `"logodds"` are the
same curve on the odds and log-odds scales. The `causal` curve is a
contrast (below) and is *not* shown on `"prob"`/`"odds"`; use
`"logodds"` to see it.

## Reading a survival-probability curve (scale = "surv")

The y-axis is \\S(\tau \mid x)\\, the predicted probability of surviving
past \\\tau\\, bounded in \\\[0, 1\]\\ and read in the model's time
units. Higher is better (more survival). \\\tau\\ defaults to the median
follow-up time when not supplied.

## What the causal curve is, and when to use it

`causal` is the **baseline-subtracted local effect** – varPro's virtual-
("digital-") twins estimator (Ishwaran & Blackstone, 2025). It shows how
the prediction shifts as the focal variable moves away from the
reference grid point, with the other covariates held at on-manifold
(UVT-plausible) values; it is a **contrast** (it starts at 0), not a
level. Use it when you want the local effect (change-from-baseline)
rather than the absolute predicted level, and as a cross-check on the
parametric and nonparametric curves. It is varpro's local estimator
*within the fitted model*, **not a structural causal claim** about the
data-generating process. Because it is a contrast it cannot share a
probability/odds axis with the absolute curves, so it is shown only on
the additive scales (`"logodds"`, `"mortality"`, `"rmst"`).

## Reading an RMST curve (scale = "rmst")

The y-axis is restricted mean survival time at horizon \\\tau\\,
\\\mathrm{RMST}(\tau)=\int_0^\tau S(t)\\dt\\: the **expected event-free
time during the first \\\tau\\ time-units**, the area under the survival
curve out to \\\tau\\. Read it in the **model's own time units**, where
it is bounded by \\0 \le \mathrm{RMST}(\tau) \le \tau\\.

Two things follow. First, \\\tau\\ must be given in the fit's time
units; a \\\tau\\ past the largest event time just truncates to the full
restricted mean and stops changing. Second, higher is better here – more
time event-free – which is the opposite of the ensemble-mortality scale.

A continuous variable's curve sloping *up* means higher values of that
covariate buy you *more* restricted-mean event-free time within \\\tau\\
(with the other covariates held at their UVT-plausible average); a flat
curve means the covariate does not move it. Unlike ensemble mortality,
RMST reads on a directly clinical scale, "so many event-free time-units
within \\\tau\\", which is usually the one you want to report.

## References

Ishwaran H, Kogalur UB, Blackstone EH, Lauer MS (2008). Random survival
forests. *The Annals of Applied Statistics*, **2**(3), 841–860.
[doi:10.1214/08-AOAS169](https://doi.org/10.1214/08-AOAS169) .

Ishwaran H, Blackstone EH (2025). Harnessing the power of virtual
(digital) twins: Graphical causal tools for understanding patient and
hospital differences. *Computational and Structural Biotechnology
Journal*, **28**, 312.

## See also

[`gg_partial_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)

## Examples

``` r
set.seed(42)
n_obs <- 30; n_pts <- 15
mock_data <- list(
  age = list(
    xvirtual    = seq(30, 80, length.out = n_pts),
    xorg        = sample(seq(30, 80, by = 5), n_obs, replace = TRUE),
    yhat.par    = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
    yhat.nonpar = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
    yhat.causal = matrix(rnorm(n_obs * n_pts), nrow = n_obs)
  ),
  sex = list(
    xvirtual    = c(0, 1),
    xorg        = sample(c(0, 1), n_obs, replace = TRUE),
    yhat.par    = matrix(rnorm(n_obs * 2), nrow = n_obs),
    yhat.nonpar = matrix(rnorm(n_obs * 2), nrow = n_obs),
    yhat.causal = matrix(rnorm(n_obs * 2), nrow = n_obs)
  )
)
pp <- gg_partial_varpro(mock_data)
plot(pp)

plot(pp, type = "parametric")

```
