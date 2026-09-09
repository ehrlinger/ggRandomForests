# Precompute the expensive objects used by the explainability vignette.
#
# Two reasons this file exists:
#
#  1. Speed. gg_shap() on the Boston forest takes about 320 seconds, and
#     gg_partial_varpro() about 21. Everything else in the vignette runs in
#     under three seconds combined, so these two are the whole cost. CRAN
#     rejects a package whose overall R CMD check exceeds about ten minutes,
#     and the vignette rebuild is already the largest step.
#  2. Safety. Every varPro grow reaches randomForestSRC's compiled rule-grow
#     path, which trips a gcc-UBSAN "0-length array" report (rfsrcGrow,
#     entry.c:184). Loading the derived object from disk means the vignette
#     performs no live varPro grow during R CMD check, so it cannot surface
#     that upstream sanitizer report. This is the same reason
#     precompute_varpro.R exists.
#
# What is NOT stored, and why: the rfsrc and varpro fits themselves. Both are
# about 5 MB, the tarball has roughly 1.4 MB of headroom against CRAN's 5 MB
# limit, and the rfsrc fit takes 0.2 seconds to rebuild. Only the small
# derived objects travel.
#
# Run from the package root:
#   Rscript vignettes/precompute_explainability.R

# Match precompute_varpro.R / precompute_rhf.R: try the installed package,
# fall back to pkgload::load_all() so this runs in a fresh clone before
# installation, and fail with a message that names the missing piece rather
# than whatever error the first unguarded call happens to throw.
if (requireNamespace("ggRandomForests", quietly = TRUE)) {
  suppressMessages(library(ggRandomForests))
} else if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(export_all = FALSE, helpers = FALSE,
                    attach_testthat = FALSE)
} else {
  stop("Install ggRandomForests (or pkgload for dev builds) to run this script.")
}

for (pkg in c("MASS", "randomForestSRC", "kernelshap", "varPro")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Install '", pkg, "' to run this script.", call. = FALSE)
  }
}

data(Boston, package = "MASS")

set.seed(20260909L)
rf <- randomForestSRC::rfsrc(medv ~ ., data = Boston, ntree = 500)

# SHAP on a 60-row subsample. The full 506 rows cost 320 seconds and 362 KB;
# the beeswarm reads the same at 60 and the object drops to a fraction of
# that. A SHAP plot is about the spread of attributions, not the census.
# newdata takes PREDICTORS only. Passing a frame that still carries the
# response fails inside kernelshap with
# "all(colnames(X) %in% colnames(bg_X)) is not TRUE", which names neither
# the offending column nor gg_shap(). rf$xvar is the training predictors.
set.seed(20260909L)
shap_rows <- sort(sample(nrow(Boston), 60))
shap_boston <- gg_shap(rf, newdata = rf$xvar[shap_rows, , drop = FALSE],
                       bg_n = 30)

# split.weight = FALSE keeps every predictor reachable for partialpro(); see
# ?gg_partial_varpro and the varpro vignette for why the screen matters.
set.seed(20260909L)
v_boston <- varPro::varpro(medv ~ ., data = Boston, split.weight = FALSE)
pd_varpro <- gg_partial_varpro(object = v_boston)

saveRDS(
  list(shap_boston = shap_boston, pd_varpro = pd_varpro),
  file.path("vignettes", "explainability_precomputed.rds"),
  compress = "xz"
)

message("wrote vignettes/explainability_precomputed.rds")
