# Precompute the varPro objects used by the varpro vignette.
#
# Two reasons this file exists:
#
#  1. Speed. The three gg_partial_varpro() calls (~31s) and the beta.varpro()
#     fits dominate the vignette rebuild time (CRAN flagged the overall check
#     time).
#  2. Safety. Every varPro grow (varpro(), uvarpro(), isopro(), ivarpro(),
#     beta.varpro()) reaches randomForestSRC's compiled rule-grow path, which
#     trips a gcc-UBSAN "0-length array" report (rfsrcGrow, entry.c:184). By
#     loading these objects from disk, the vignette performs NO live varPro
#     grow during R CMD check, so it cannot surface that upstream sanitizer
#     issue even if a CRAN flavour builds the vignette under UBSAN.
#
# Run from the package root after changing any of the varpro vignette fits:
#   Rscript vignettes/precompute_varpro.R
#
# Produces vignettes/varpro_precomputed.rds. The vignette falls back to live
# computation for any object missing from that file, so the result is
# reproducible either way.

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
boston_x <- Boston[, setdiff(names(Boston), "medv")]
set.seed(20260527L)
v_boston   <- varPro::varpro(medv ~ ., data = Boston, ntree = 50)
pd_boston  <- gg_partial_varpro(object = v_boston)
b_boston   <- varPro::beta.varpro(v_boston)
iv_boston  <- varPro::ivarpro(v_boston)
set.seed(20260527L)
u_boston   <- varPro::uvarpro(boston_x, ntree = 50)
set.seed(20260527L)
iso_boston <- varPro::isopro(data = boston_x, method = "rnd",
                             sampsize = 256, ntree = 50)

# --- Classification: iris ------------------------------------------------
iris_binary <- iris[iris$Species != "setosa", ]
iris_binary$Species <- droplevels(iris_binary$Species)
set.seed(20260527L)
v_iris_binary <- varPro::varpro(Species ~ ., data = iris_binary, ntree = 50)
b_iris_binary <- varPro::beta.varpro(v_iris_binary)
set.seed(20260527L)
v_iris_multi  <- varPro::varpro(Species ~ ., data = iris, ntree = 50)
pd_iris_multi <- gg_partial_varpro(object = v_iris_multi)
b_iris_multi  <- varPro::beta.varpro(v_iris_multi)

# --- Survival: PBC (C-path partial dependence) ---------------------------
data(pbc, package = "randomForestSRC")
pbc_small <- na.omit(pbc[, c("days", "status", "age", "albumin", "bili",
                             "edema", "platelet")])
set.seed(20260527L)
v_pbc   <- varPro::varpro(Surv(days, status) ~ ., data = pbc_small, ntree = 50)
pd_pbc  <- gg_partial_varpro(object = v_pbc)
set.seed(20260527L)
iso_pbc <- varPro::isopro(data = pbc_small[, c("age", "albumin", "bili",
                                               "platelet")],
                          method = "rnd", sampsize = 256, ntree = 50)

# --- Trim to keep the shipped .rds (and the source tarball) small --------
# The gg_* wrappers that consume these objects in the vignette only read
# importance/rule summaries, never the embedded forests. Dropping those heavy
# slots takes the file from ~1.6 MB to ~0.4 MB (validated: every vignette
# wrapper call returns output identical to the un-stripped object). Two
# exceptions keep their forest: v_boston is printed in the vignette
# (print.varpro reads $rf), and u_boston feeds gg_udependent(), which uses it.
.strip_varpro  <- function(v)  { v$rf <- NULL; v }                       # forest unused by gg_*
.strip_isopro  <- function(o)  { o$isoforest <- list(ntree = o$isoforest$ntree); o }  # gg_isopro reads only $ntree
.strip_ivarpro <- function(iv) {                                         # drop redundant model/path attrs
  for (a in c("ivarpro.path", "model", "data")) attr(iv, a) <- NULL
  iv
}
b_boston      <- .strip_varpro(b_boston)
v_pbc         <- .strip_varpro(v_pbc)
v_iris_binary <- .strip_varpro(v_iris_binary)
v_iris_multi  <- .strip_varpro(v_iris_multi)
iso_boston    <- .strip_isopro(iso_boston)
iso_pbc       <- .strip_isopro(iso_pbc)
iv_boston     <- .strip_ivarpro(iv_boston)

saveRDS(
  list(
    # Boston (regr)
    v_boston      = v_boston,
    pd_boston     = pd_boston,
    b_boston      = b_boston,
    iv_boston     = iv_boston,
    u_boston      = u_boston,
    iso_boston    = iso_boston,
    # iris (class)
    v_iris_binary = v_iris_binary,
    b_iris_binary = b_iris_binary,
    v_iris_multi  = v_iris_multi,
    pd_iris_multi = pd_iris_multi,
    b_iris_multi  = b_iris_multi,
    # PBC (surv)
    v_pbc         = v_pbc,
    pd_pbc        = pd_pbc,
    iso_pbc       = iso_pbc
  ),
  file = "vignettes/varpro_precomputed.rds",
  version = 2,
  compress = "xz"
)

cat("Wrote vignettes/varpro_precomputed.rds (",
    round(file.size("vignettes/varpro_precomputed.rds") / 1024, 1), "KB )\n")
