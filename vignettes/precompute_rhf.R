# Precompute the randomForestRHF objects used by the RHF vignette.
#
# Run from the package root:
#   Rscript vignettes/precompute_rhf.R

suppressPackageStartupMessages(library(survival))

if (!requireNamespace("randomForestRHF", quietly = TRUE)) {
  stop("Install 'randomForestRHF' to prepare the RHF vignette bundle.")
}

artifact_path <- file.path("vignettes", "rhf_precomputed.rds")
seed <- 20260825L
set.seed(seed)
sim <- randomForestRHF::hazard.simulation(1)
data <- sim$dta
formula <- stats::as.formula("Surv(id, start, stop, event) ~ .")

fit <- randomForestRHF::rhf(
  formula, data, ntree = 50L, seed = -1L
)
auct_cumulative <- randomForestRHF::auct.rhf(
  fit, marker = "cumhaz", method = "cumulative", verbose = FALSE
)
auct_incident <- randomForestRHF::auct.rhf(
  fit, marker = "hazard", method = "incident",
  riskset = "subject", verbose = FALSE
)
cache <- randomForestRHF::varpro.cache.rhf(
  fit, max.rules.tree = 30L, max.tree = 20L, verbose = FALSE
)
time_index <- unique(as.integer(round(seq.int(1L, cache$K, length.out = 5L))))
importance <- randomForestRHF::importance.rhf(
  fit, cache = cache, time.index = time_index, verbose = FALSE
)
tune_risk <- randomForestRHF::tune.treesize.rhf(
  formula, data, ntree = 20L, perf = "risk", lower = 2L,
  upper = 6L, max.evals = 5L, seed = seed, verbose = FALSE,
  forest = FALSE
)
tune_iauc <- randomForestRHF::tune.iAUC.rhf(
  formula, data, ntree = 20L, lower = 2L, upper = 6L,
  max.evals = 5L, seed = seed, verbose = FALSE, forest = FALSE
)

settings <- list(
  formula = "Surv(id, start, stop, event) ~ .",
  fit = list(ntree = 50L, seed = -1L),
  auct_cumulative = list(
    marker = "cumhaz", method = "cumulative", verbose = FALSE
  ),
  auct_incident = list(
    marker = "hazard", method = "incident", riskset = "subject",
    verbose = FALSE
  ),
  importance_cache = list(
    max.rules.tree = 30L, max.tree = 20L, verbose = FALSE
  ),
  importance_time_index = time_index,
  tune_risk = list(
    ntree = 20L, perf = "risk", lower = 2L, upper = 6L, max.evals = 5L,
    seed = seed, verbose = FALSE, forest = FALSE
  ),
  tune_iauc = list(
    ntree = 20L, lower = 2L, upper = 6L, max.evals = 5L, seed = seed,
    verbose = FALSE, forest = FALSE
  )
)
versions <- c(
  R = as.character(getRversion()),
  ggRandomForests = unname(read.dcf("DESCRIPTION")[1L, "Version"]),
  randomForestRHF = as.character(utils::packageVersion("randomForestRHF")),
  ggplot2 = as.character(utils::packageVersion("ggplot2"))
)
bundle <- list(
  data = data,
  fit = fit,
  auct_cumulative = auct_cumulative,
  auct_incident = auct_incident,
  importance = importance,
  tune_risk = tune_risk,
  tune_iauc = tune_iauc,
  seed = seed,
  settings = settings,
  versions = versions
)

stopifnot(
  identical(names(bundle), c(
    "data", "fit", "auct_cumulative", "auct_incident", "importance",
    "tune_risk", "tune_iauc", "seed", "settings", "versions"
  )),
  identical(data$xtd, (data$x.4 + data$x.5) * data$stop),
  inherits(fit, "rhf"),
  inherits(tune_risk, "tune.treesize.rhf"),
  inherits(tune_iauc, "tune.treesize.rhf"),
  !"forest" %in% names(tune_risk),
  !"forest" %in% names(tune_iauc),
  length(time_index) == 5L,
  all(time_index >= 1L & time_index <= cache$K),
  length(unique(time_index)) == 5L
)

temporary_path <- tempfile(
  pattern = "rhf_precomputed-", tmpdir = dirname(artifact_path),
  fileext = ".rds"
)
on.exit(unlink(temporary_path), add = TRUE)
saveRDS(bundle, temporary_path, version = 2, compress = "xz")
stopifnot(file.info(temporary_path)$size <= 1.75 * 1024^2)

verified_bundle <- readRDS(temporary_path)
stopifnot(
  identical(names(verified_bundle), names(bundle)),
  inherits(verified_bundle$fit, "rhf"),
  inherits(verified_bundle$tune_risk, "tune.treesize.rhf"),
  inherits(verified_bundle$tune_iauc, "tune.treesize.rhf"),
  !"forest" %in% names(verified_bundle$tune_risk),
  !"forest" %in% names(verified_bundle$tune_iauc)
)

if (!file.copy(temporary_path, artifact_path, overwrite = TRUE)) {
  stop("Could not place the verified RHF vignette bundle.")
}
stopifnot(identical(
  unname(tools::md5sum(temporary_path)),
  unname(tools::md5sum(artifact_path))
))
stopifnot(unlink(temporary_path) == 0L)

cat(
  "Wrote ", artifact_path, " (",
  round(file.size(artifact_path) / 1024, 1), " KB)\n",
  sep = ""
)
