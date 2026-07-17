# Variable Importance (VIMP) data object

`gg_vimp` Extracts the variable importance (VIMP) information from a
[`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html) or
[`randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
object and reshapes it into a tidy data set.

## Usage

``` r
gg_vimp(object, nvar, ...)
```

## Arguments

- object:

  A [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html)
  object, the output from
  [`vimp`](https://www.randomforestsrc.org//reference/vimp.rfsrc.html),
  or a fitted
  [`randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html).

- nvar:

  argument to control the number of variables included in the output.

- ...:

  arguments passed to the
  [`vimp.rfsrc`](https://www.randomforestsrc.org//reference/vimp.rfsrc.html)
  function if the
  [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html)
  object does not contain importance information.

## Value

`gg_vimp` object. A `data.frame` of VIMP measures, in rank order,
optionally containing class-specific scores and a relative importance
column. When `randomForest` objects lack stored importance values a
warning is issued and `NA` placeholders are returned so plots remain
reproducible.

## Details

`gg_vimp()` reports whatever importance the forest stored; it computes
nothing itself. Usually that is **permutation (Breiman-Cutler) variable
importance**: the forest permutes a variable's observed values across
the out-of-bag (OOB) cases, runs those perturbed cases down the
already-grown trees, and measures how much the OOB prediction error
climbs. That perturbation is synthetic (the variable's link to the
response is broken on purpose) so a large increase means the variable
was carrying genuine signal; near-zero or negative values mean it added
noise or nothing at all.

**A `randomForest` fit needs `importance = TRUE` to give you this.**
[`randomForest::randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
defaults to `importance = FALSE`, and that fit stores only
`IncNodePurity` – a node-impurity (RSS or Gini) measure, which is not a
permutation quantity and is not comparable to one. It is the only
importance the forest kept, so it is what `gg_vimp()` reports, in the
`vimp` column, same as any other. Nothing marks the difference in the
plot. So `gg_vimp(randomForest(y ~ ., data))` ranks by node purity; pass
`importance = TRUE` and you get permutation VIMP (`%IncMSE`), and
`colnames(object$importance)` tells you which one you have.
[`randomForestSRC::rfsrc()`](https://www.randomforestsrc.org//reference/rfsrc.html)
has no such trap: its `importance` argument yields permutation VIMP.

When a `randomForest` fit carries both measures, `gg_vimp()` reports the
permutation one and leaves node purity out of the ranking – the two run
on different scales and mean different things, so putting them in one
ordering would be meaningless. Read `randomForest::importance(object)`
if you want both. A classification fit names that pair
`MeanDecreaseAccuracy` and `MeanDecreaseGini`, and stores a permutation
column per class besides. Those per-class columns are all permutation
measures on one scale, so `gg_vimp()` keeps them together, names each in
the `set` column, and drops only the Gini one.

[`gg_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md)
takes the opposite route, comparing local estimators on real observed
data through varPro's release rules, with no permutation and no
synthetic features. The two approaches answer "which variables matter?"
by opposite mechanisms, so a variable can rank differently under each,
and that disagreement is itself informative: it often signals
interaction structure or non-monotone effects that one mechanism
surfaces and the other obscures.

For survival forests, VIMP is measured against the ensemble cumulative
hazard function (CHF); the error metric is one minus the concordance
index (C-statistic). Variables with non-positive VIMP are flagged in the
`positive` column and colored differently by
[`plot.gg_vimp`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_vimp.md).

## References

Ishwaran H. (2007). Variable importance in binary regression trees and
forests, *Electronic J. Statist.*, 1:519-537.

## See also

[`plot.gg_vimp`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_vimp.md)
[`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html)

[`vimp`](https://www.randomforestsrc.org//reference/vimp.rfsrc.html)
[`gg_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md)

## Examples

``` r
## ------------------------------------------------------------
## classification example
## ------------------------------------------------------------
## -------- iris data
rfsrc_iris <- randomForestSRC::rfsrc(Species ~ .,
  data = iris,
  importance = TRUE
)
gg_dta <- gg_vimp(rfsrc_iris)
plot(gg_dta)


## ------------------------------------------------------------
## regression example
## ------------------------------------------------------------

## -------- air quality data
rfsrc_airq <- randomForestSRC::rfsrc(Ozone ~ ., airquality,
  importance = TRUE
)
gg_dta <- gg_vimp(rfsrc_airq)
plot(gg_dta)



## -------- Boston data
data(Boston, package = "MASS")
rfsrc_boston <- randomForestSRC::rfsrc(medv ~ ., Boston,
  importance = TRUE
)
gg_dta <- gg_vimp(rfsrc_boston)
plot(gg_dta)


## -------- Boston data
## importance = TRUE for permutation VIMP; without it randomForest stores
## only IncNodePurity, which is what you would be ranking (see Details).
rf_boston <- randomForest::randomForest(medv ~ ., Boston, importance = TRUE)
gg_dta <- gg_vimp(rf_boston)
plot(gg_dta)



## -------- mtcars data
rfsrc_mtcars <- randomForestSRC::rfsrc(mpg ~ .,
  data = mtcars,
  importance = TRUE
)
gg_dta <- gg_vimp(rfsrc_mtcars)
plot(gg_dta)


## ------------------------------------------------------------
## survival example
## ------------------------------------------------------------

## -------- veteran data
data(veteran, package = "randomForestSRC")
rfsrc_veteran <- randomForestSRC::rfsrc(Surv(time, status) ~ .,
  data = veteran,
  ntree = 100,
  importance = TRUE
)

gg_dta <- gg_vimp(rfsrc_veteran)
plot(gg_dta)


## -------- pbc data
# We need to create this dataset
data(pbc, package = "randomForestSRC", )
#> Warning: data set ‘’ not found
# For whatever reason, the age variable is in days...
# makes no sense to me
for (ind in seq_len(dim(pbc)[2])) {
  if (!is.factor(pbc[, ind])) {
    if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
      if (sum(range(pbc[, ind], na.rm = TRUE) == c(0, 1)) == 2) {
        pbc[, ind] <- as.logical(pbc[, ind])
      }
    }
  } else {
    if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
      if (sum(sort(unique(pbc[, ind])) == c(0, 1)) == 2) {
        pbc[, ind] <- as.logical(pbc[, ind])
      }
      if (sum(sort(unique(pbc[, ind])) == c(FALSE, TRUE)) == 2) {
        pbc[, ind] <- as.logical(pbc[, ind])
      }
    }
  }
  if (!is.logical(pbc[, ind]) &
    length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 5) {
    pbc[, ind] <- factor(pbc[, ind])
  }
}
# Convert age to years
pbc$age <- pbc$age / 364.24

pbc$years <- pbc$days / 364.24
pbc <- pbc[, -which(colnames(pbc) == "days")]
pbc$treatment <- as.numeric(pbc$treatment)
pbc$treatment[which(pbc$treatment == 1)] <- "DPCA"
pbc$treatment[which(pbc$treatment == 2)] <- "placebo"
pbc$treatment <- factor(pbc$treatment)
dta_train <- pbc[-which(is.na(pbc$treatment)), ]
# Create a test set from the remaining patients
pbc_test <- pbc[which(is.na(pbc$treatment)), ]

# ========
# build the forest:
rfsrc_pbc <- randomForestSRC::rfsrc(
  Surv(years, status) ~ .,
  dta_train,
  nsplit = 10,
  na.action = "na.impute",
  forest = TRUE,
  importance = TRUE,
  save.memory = TRUE
)

gg_dta <- gg_vimp(rfsrc_pbc)
plot(gg_dta)


# Restrict to only the top 10.
gg_dta <- gg_vimp(rfsrc_pbc, nvar = 10)
plot(gg_dta)

```
