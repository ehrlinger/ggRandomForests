# Precompute the expensive varPro objects used by the varpro vignette.
#
# The three gg_partial_varpro() calls (~31s) and the Boston beta.varpro()
# fit (~3s) dominate the vignette rebuild time. Computing them once here and
# loading the result in varpro.qmd keeps the R CMD check vignette rebuild
# fast (CRAN flagged the overall check time). Every other varPro call in the
# vignette is sub-second and stays live.
#
# Run from the package root after changing any of the varpro vignette fits:
#   Rscript vignettes/precompute_varpro.R
#
# Produces vignettes/varpro_precomputed.rds. The vignette falls back to live
# computation if that file is absent, so the result is reproducible either way.

suppressMessages({
  library(varPro)
  library(survival)
})
# Mirror the vignette's loader: prefer the installed package, fall back to
# pkgload::load_all() so this runs in a fresh clone before installation.
if (requireNamespace("ggRandomForests", quietly = TRUE)) {
  suppressMessages(library(ggRandomForests))
} else if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(export_all = FALSE, helpers = FALSE,
                    attach_testthat = FALSE)
} else {
  stop("Install ggRandomForests (or pkgload for dev builds) to run this script.")
}
options(mc.cores = 1, rf.cores = 1)

# --- Regression: Boston housing ------------------------------------------
data("Boston", package = "MASS")
set.seed(20260527L)
v_boston  <- varPro::varpro(medv ~ ., data = Boston, ntree = 50)
pd_boston <- gg_partial_varpro(object = v_boston)
b_boston  <- varPro::beta.varpro(v_boston)

# --- Classification: iris (multi-class) ----------------------------------
set.seed(20260527L)
v_iris_multi  <- varPro::varpro(Species ~ ., data = iris, ntree = 50)
pd_iris_multi <- gg_partial_varpro(object = v_iris_multi)

# --- Survival: PBC (C-path partial dependence) ---------------------------
data(pbc, package = "randomForestSRC")
pbc_small <- na.omit(pbc[, c("days", "status", "age", "albumin", "bili",
                             "edema", "platelet")])
set.seed(20260527L)
v_pbc  <- varPro::varpro(Surv(days, status) ~ ., data = pbc_small, ntree = 50)
pd_pbc <- gg_partial_varpro(object = v_pbc)

saveRDS(
  list(
    pd_boston     = pd_boston,
    b_boston      = b_boston,
    pd_iris_multi = pd_iris_multi,
    pd_pbc        = pd_pbc
  ),
  file = "vignettes/varpro_precomputed.rds",
  version = 2,
  compress = "xz"
)

cat("Wrote vignettes/varpro_precomputed.rds (",
    round(file.size("vignettes/varpro_precomputed.rds") / 1024, 1), "KB )\n")
