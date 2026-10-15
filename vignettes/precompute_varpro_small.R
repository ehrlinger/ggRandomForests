# Precompute the objects used by the varpro-small-data vignette.
#
# Same reasons as precompute_varpro.R: the fits dominate the rebuild time, and
# loading them from disk means the vignette performs no live varPro or
# randomForestSRC grow during R CMD check (the gcc-UBSAN report in rfsrcGrow).
#
# Run from the package root after changing any fit below:
#   Rscript vignettes/precompute_varpro_small.R
#
# Produces vignettes/varpro_small_precomputed.rds. If that file is missing,
# the vignette sources this script (without saving) and computes live.

suppressMessages({
  library(varPro)
  library(survival)
})
if (requireNamespace("ggRandomForests", quietly = TRUE)) {
  suppressMessages(library(ggRandomForests))
} else if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(export_all = FALSE, helpers = FALSE,
                    attach_testthat = FALSE)
} else {
  stop("Install ggRandomForests (or pkgload for dev builds) to run this script.")
}
options(mc.cores = 1, rf.cores = 1)

# --- Data: PBC, complete cases, factors as factors, plus permuted noise ------
pbc_small_data <- function() {
  data(pbc, package = "randomForestSRC", envir = environment())
  d <- na.omit(pbc)
  for (v in c("treatment", "sex", "ascites", "hepatom", "spiders", "edema", "stage")) {
    d[[v]] <- factor(d[[v]])
  }
  ## Ten permuted copies of real columns: same values, shuffled between
  ## patients, so unrelated to survival by construction. Five continuous,
  ## five categorical, so each kind of variable has noise of its own kind.
  src <- c("age", "albumin", "bili", "chol", "platelet",
           "sex", "stage", "edema", "spiders", "ascites")
  for (i in seq_along(src)) {
    set.seed(100L + i)
    d[[paste0("noise", i, "_", src[i])]] <- sample(d[[src[i]]])
  }
  d
}
d <- pbc_small_data()
f <- Surv(days, status) ~ .
tau <- 1461   # 4 years, in days

# --- Two screens: varPro's defaults, and every variable offered ---------------
set.seed(2026L)
vp_guided <- varPro::varpro(f, data = d, ntree = 1000, seed = -2026L)
set.seed(2026L)
vp_open <- varPro::varpro(f, data = d, ntree = 1000, seed = -2026L,
                          split.weight = FALSE, mtry = 4)

cov_guided_t <- varpro_coverage(vp_guided, seed = -1L, noise = "^noise")
cov_guided_v <- suppressWarnings(varpro_coverage(vp_guided, by = "variable",
                                                 seed = -1L, noise = "^noise"))
cov_open_v   <- varpro_coverage(vp_open, by = "variable", seed = -1L, noise = "^noise")

set.seed(3L)
gg_guided_150 <- gg_varpro(vp_guided)
gg_guided_all <- gg_varpro(vp_guided, max.tree = vp_guided$rf$ntree)

trims <- c(0.1, 0.05, 0.025, 0)
trim_z <- lapply(trims, function(tr) {
  r <- varpro_coverage(vp_guided, trim = tr, seed = -1L)
  r[!is.na(r$z), c("term", "z", "coverage")]
})
names(trim_z) <- trims

set.seed(2026L)
cv_guided <- varPro::cv.varpro(f, data = d, ntree = 1000, seed = -2026L)
cv_zcut <- cv_guided$zcut

# --- Partial dependence: local (varPro) against marginal (randomForestSRC) ----
real <- d[, !grepl("^noise", names(d))]
set.seed(2026L)
rf_pbc <- randomForestSRC::rfsrc(f, data = real, ntree = 1000, seed = -2026L)
set.seed(2026L)
## varPro one-hot encodes a factor (stage becomes stage1 ... stage4), and
## partialpro() looks terms up by those encoded names; asking for "stage"
## skips it with a warning.
fac_terms <- grep("^(stage|edema)", vp_open$xvar.names, value = TRUE)
pd_local <- gg_partial_varpro(object = vp_open, scale = "surv", time = tau,
                              xvar.names = c("albumin", "bili", fac_terms))
pd_marginal <- gg_partial_rfsrc(rf_pbc, xvar.names = c("albumin", "bili"),
                                partial.time = tau)

# Horizons before the first death: randomForestSRC evaluates survival on its
# event-time grid, so an earlier request returns the first event's curve.
first_death <- min(real$days[real$status == 1])
pd_early <- gg_partial_rfsrc(rf_pbc, xvar.names = "bili",
                             partial.time = c(10, 30, first_death, 365))

# Refits on subsamples: how far the curve itself moves when the forest is
# refitted, to set beside the per-case band of one forest.
pd_refits <- lapply(1:5, function(r) {
  set.seed(500L + r)
  rows <- sample.int(nrow(real), floor(0.632 * nrow(real)))
  rf_r <- randomForestSRC::rfsrc(f, data = real[rows, ], ntree = 500, seed = -(500L + r))
  out <- gg_partial_rfsrc(rf_r, xvar.names = "bili", partial.time = tau)
  out$refit <- r
  out
})

# Observed survival at tau for stage 4 against the rest, to check the level
# partials against (Kaplan-Meier, no forest).
km <- summary(survival::survfit(Surv(days, status) ~ I(stage == "4"), data = real), times = tau)$surv
km_stage4 <- c(`other stages` = km[[1]], `stage 4` = km[[2]])

# partialpro() sets one level column and leaves the factor's other level
# columns as they were, so a stage 3 case "set to stage4 = 1" is stage 3 and
# stage 4 at once. How much does that matter here? Compare the forest's own
# mean survival at tau for stage4 = 1 with siblings left alone against the
# whole factor set to stage 4 (a valid profile).
surv_tau <- function(newx) {
  p <- randomForestSRC::predict.rfsrc(vp_open$rf, newx, perf.type = "none")
  mean(p$survival[, max(which(p$time.interest <= tau))])
}
st_cols <- grep("^stage", colnames(vp_open$x), value = TRUE)
x_sib <- vp_open$x
x_sib$stage4 <- 1
x_valid <- vp_open$x
x_valid[st_cols] <- 0
x_valid$stage4 <- 1
sibling_check <- c(siblings_left = surv_tau(x_sib), valid_profile = surv_tau(x_valid))

varpro_small_objects <- list(
  n = nrow(d), deaths = sum(d$status == 1), n_real = ncol(real) - 2L,
  tau = tau, first_death = first_death, fac_terms = fac_terms, km_stage4 = km_stage4, sibling_check = sibling_check,
  cov_guided_t = cov_guided_t, cov_guided_v = cov_guided_v, cov_open_v = cov_open_v,
  gg_guided_150 = gg_guided_150, gg_guided_all = gg_guided_all,
  trim_z = trim_z, cv_zcut = cv_zcut,
  pd_local = pd_local, pd_marginal = pd_marginal, pd_early = pd_early,
  pd_refits = pd_refits
)
## The vignette's live fallback sources this file with .varpro_small_no_save
## set, to get the objects without writing the .rds.
if (!exists(".varpro_small_no_save")) {
  saveRDS(varpro_small_objects, file.path("vignettes", "varpro_small_precomputed.rds"),
          compress = "xz")
}
