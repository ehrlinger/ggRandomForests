# Release-only execution probe for native code in the three upstream engines.

.ubsan_pattern <- "runtime error:|UndefinedBehaviorSanitizer"

assert_ubsan_clean <- function(path) {
  output <- readLines(path, warn = FALSE)
  if (any(grepl(.ubsan_pattern, output, perl = TRUE))) {
    stop(
      "Supported workflow emitted an UndefinedBehaviorSanitizer diagnostic.",
      call. = FALSE
    )
  }
  invisible()
}

assert_known_rfsrc_ubsan <- function(path) {
  output <- readLines(path, warn = FALSE)
  known <- grepl(
    "entry\\.c:[0-9]+.*runtime error: pointer index expression",
    output,
    perl = TRUE
  )
  if (!any(known)) {
    stop("Did not observe the known randomForestSRC UBSAN diagnostic.",
         call. = FALSE)
  }
  invisible()
}

run_known_rfsrc_probe <- function() {
  randomForestSRC::rfsrc(
    randomForestSRC::Unsupervised() ~ .,
    data = mtcars,
    ntree = 1L
  )
  stop("The known randomForestSRC UBSAN probe unexpectedly returned.",
       call. = FALSE)
}

run_supported_workflows <- function() {
  set.seed(20260826L)
  rfsrc_fit <- randomForestSRC::rfsrc(mpg ~ ., data = mtcars, ntree = 20L)
  rfsrc_view <- ggRandomForests::gg_rfsrc(rfsrc_fit)
  stopifnot(
    inherits(rfsrc_fit, "rfsrc"),
    inherits(rfsrc_view, "gg_rfsrc"),
    inherits(plot(rfsrc_view), "ggplot")
  )

  set.seed(20260826L)
  varpro_fit <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 20L)
  varpro_view <- ggRandomForests::gg_varpro(varpro_fit)
  stopifnot(
    inherits(varpro_fit, "varpro"),
    inherits(varpro_view, "gg_varpro"),
    inherits(plot(varpro_view), "ggplot")
  )

  simulated <- randomForestRHF::hazard.simulation(1)
  rhf_formula <- "Surv(id, start, stop, event) ~ ."
  set.seed(20260826L)
  rhf_fit <- randomForestRHF::rhf(
    rhf_formula,
    simulated$dta,
    ntree = 20L,
    seed = -1L
  )
  rhf_prediction <- predict(rhf_fit)

  cumulative_auc <- randomForestRHF::auct.rhf(
    rhf_fit,
    marker = "cumhaz",
    method = "cumulative",
    verbose = FALSE
  )
  incident_auc <- randomForestRHF::auct.rhf(
    rhf_fit,
    marker = "hazard",
    method = "incident",
    riskset = "subject",
    verbose = FALSE
  )

  priority_cache <- randomForestRHF::varpro.cache.rhf(
    rhf_fit,
    max.rules.tree = 10L,
    max.tree = 5L,
    verbose = FALSE
  )
  time_index <- unique(as.integer(round(seq.int(
    1L,
    priority_cache$K,
    length.out = 3L
  ))))
  priority_fit <- randomForestRHF::importance.rhf(
    rhf_fit,
    cache = priority_cache,
    time.index = time_index,
    verbose = FALSE
  )

  tune_fit <- randomForestRHF::tune.iAUC.rhf(
    rhf_formula,
    simulated$dta,
    ntree = 10L,
    lower = 2L,
    upper = 4L,
    max.evals = 3L,
    seed = 20260826L,
    verbose = FALSE,
    forest = FALSE
  )

  rhf_view <- ggRandomForests::gg_rhf(rhf_fit)
  auc_view <- ggRandomForests::gg_auct(
    rhf_fit,
    marker = "chf",
    auct_fit = cumulative_auc
  )
  priority_view <- ggRandomForests::gg_rhf_importance(
    rhf_fit,
    importance_fit = priority_fit
  )
  tune_view <- ggRandomForests::gg_tune_rhf(tune_fit)
  stopifnot(
    inherits(rhf_fit, "rhf"),
    inherits(rhf_prediction, "rhf"),
    inherits(cumulative_auc, "auct.rhf"),
    inherits(incident_auc, "auct.rhf"),
    inherits(priority_fit, "importance.rhf"),
    inherits(tune_fit, "tune.treesize.rhf"),
    inherits(plot(rhf_view, idx = 1L), "ggplot"),
    inherits(plot(auc_view), "ggplot"),
    inherits(plot(priority_view), "ggplot"),
    inherits(plot(tune_view), "ggplot")
  )

  message("Supported rfsrc, varpro, and RHF UBSAN workflows completed.")
  invisible()
}

if (!identical(Sys.getenv("GGRF_UBSAN_SOURCE_ONLY"), "true")) {
  args <- commandArgs(trailingOnly = TRUE)
  if (identical(args, "--known-rfsrc")) {
    run_known_rfsrc_probe()
  } else if (length(args) == 2L && identical(args[[1L]], "--check-clean")) {
    assert_ubsan_clean(args[[2L]])
  } else if (length(args) == 2L && identical(args[[1L]], "--check-known")) {
    assert_known_rfsrc_ubsan(args[[2L]])
  } else if (!length(args)) {
    run_supported_workflows()
  } else {
    stop("Unknown arguments for check-upstream-ubsan.R.", call. = FALSE)
  }
}
