test_that("gg_learning tidies replicates, stability and provenance", {
  x <- gg_learning(.fake_learning(c(0.5, 0.25, 1, 0.75)), top_k = 3)
  expect_s3_class(x, c("gg_learning", "data.frame"))
  expect_named(x, c("fraction", "n", "events", "replicate", "error",
                    "counts", "low_events", "optimistic"))
  expect_equal(unique(x$fraction), c(0.25, 0.5, 0.75, 1))  # sorted
  expect_equal(nrow(x), 12L)
  # 100 events in 400 cases.
  expect_equal(unique(x$events), c(25, 50, 75, 100))
  expect_equal(x$error[1:3], 0.40 - 0.025 + 0.01 * (1:3))

  st <- attr(x, "stability")
  expect_named(st, c("fraction", "n", "events", "B", "jaccard", "kendall",
                     "counts", "low_events", "optimistic"))
  expect_equal(st$counts, c("declared", "declared", "declared", "expected"))
  expect_true(all(st$jaccard >= 0 & st$jaccard <= 1))
  expect_true(all(st$kendall >= -1 & st$kendall <= 1))
  # Fraction 1 replicates have no added noise: identical rankings.
  expect_equal(st$jaccard[4], 1)
  expect_equal(st$kendall[4], 1)
  expect_equal(st$low_events, c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(st$optimistic, c(FALSE, FALSE, TRUE, FALSE))

  prov <- attr(x, "provenance")
  expect_equal(prov$family, "surv")
  expect_equal(prov$events, 100)
  expect_equal(prov$top_k, 3L)
  expect_equal(prov$B, rep(3L, 4))
  expect_equal(prov$metric, "OOB error (1 - C-index)")
})

test_that("gg_learning counts the smallest class and regression n", {
  cl <- gg_learning(.fake_learning(0.5, family = "class"))
  expect_equal(attr(cl, "provenance")$events, 134L)
  expect_equal(unique(cl$events), 67)
  rg <- gg_learning(.fake_learning(0.5, family = "regr"))
  expect_equal(unique(rg$events), 200)
  expect_equal(attr(rg, "provenance")$metric, "OOB mean squared error")
})

test_that("counts follow randomForestSRC's stratified draw: floored per stratum", {
  # 99 events and 301 censored at 0.5: 49 + 150, not round(99 / 2) = 50.
  x <- gg_learning(.fake_learning(c(0.5, 1), events = 99L), min_events = 50)
  st <- attr(x, "stability")
  expect_equal(st$events, c(49, 99))
  expect_equal(st$n, c(199, 400))
  expect_equal(st$low_events, c(TRUE, FALSE))
  # Classes of 134 and 266 at 0.25: 33 and 66.
  cl <- attr(gg_learning(.fake_learning(0.25, family = "class")), "stability")
  expect_equal(c(cl$n, cl$events), c(99, 33))
  rg <- attr(gg_learning(.fake_learning(0.125, family = "regr")), "stability")
  expect_equal(c(rg$n, rg$events), c(50, 50))
})

test_that("gg_learning refuses results from different forests of the same shape", {
  mixed <- c(.fake_learning(0.25), .fake_learning(0.5, events = 75L))
  expect_error(gg_learning(mixed), "same forest")
  other_trees <- .fake_learning(c(0.25, 0.5))
  other_trees[[2]]$rf$ntree <- 500L
  expect_error(gg_learning(other_trees), "same forest")
  # A setting that is not one of the obvious fields still counts.
  other_split <- .fake_learning(c(0.25, 0.5))
  other_split[[1]]$rf$nsplit <- 1L
  other_split[[2]]$rf$nsplit <- 20L
  expect_error(gg_learning(other_split), "same forest")
})

test_that("a recorded stratify setting overrides the declaration", {
  tag <- function(x, value) {
    lapply(x, function(s) {
      attr(s, "stratify") <- value
      s
    })
  }
  st <- attr(gg_learning(tag(.fake_learning(c(0.5, 1)), TRUE)), "stability")
  expect_equal(st$counts, c("exact", "expected"))
  expect_equal(st$low_events, c(FALSE, FALSE))
  # Recorded as unstratified: unknown, whatever the caller declares.
  st <- attr(gg_learning(tag(.fake_learning(c(0.5, 1)), FALSE),
                         stratified = TRUE), "stability")
  expect_equal(st$counts, c("expected", "expected"))
  expect_equal(st$low_events, c(NA, FALSE))
})

test_that("unstratified refits report expected counts and no event verdict", {
  x <- gg_learning(.fake_learning(c(0.125, 0.5, 1)), stratified = FALSE)
  st <- attr(x, "stability")
  expect_equal(st$counts, c("expected", "expected", "expected"))
  # Unknown for the subsamples; the bootstrap is judged at the cohort count.
  expect_equal(st$low_events, c(NA, NA, FALSE))
  expect_true(all(is.na(x$low_events[x$fraction < 1])))
  expect_true(attr(x, "provenance")$stratified == FALSE)
  p <- plot(x)
  expect_match(p$labels$x, "expected; refits not stratified", fixed = TRUE)
  built <- ggplot2::ggplot_build(p)$data[[2]]
  expect_true(4 %in% built$shape)
  expect_error(gg_learning(.fake_learning(0.5), stratified = NA), "stratified")
})

test_that("gg_learning accepts a single result and caps top_k at p", {
  expect_warning(x <- gg_learning(.fake_learning_one(0.5), top_k = 50),
                 "covers all 6 predictors")
  expect_equal(attr(x, "provenance")$top_k, 6L)
  expect_equal(nrow(attr(x, "stability")), 1L)
})

test_that("a top set of every predictor gives no Jaccard score", {
  # Fully reversed rankings: the whole set scores Jaccard 1 regardless.
  reversed <- list(c(a = 3, b = 2, c = 1), c(a = 1, b = 2, c = 3))
  expect_equal(.learning_stability(reversed, 3L)[["jaccard"]], 1)
  expect_lt(.learning_stability(reversed, 2L)[["jaccard"]], 1)
  expect_warning(st <- attr(gg_learning(.fake_learning(), top_k = 6),
                            "stability"), "top_k")
  expect_true(all(is.na(st$jaccard)))
  expect_true(all(is.finite(st$kendall)))
  expect_silent(st <- attr(gg_learning(.fake_learning(), top_k = 5),
                           "stability"))
  expect_true(all(is.finite(st$jaccard)))
})

test_that("the whole-cohort bootstrap is drawn as a square, open below min_events", {
  x <- gg_learning(.fake_learning(c(0.5, 1)), top_k = 3)
  flags <- .learning_flag_label(attr(x, "stability"))
  expect_equal(as.character(flags), c("ok", "whole cohort (expected)"))
  for (panel in c("stability", "error")) {
    shapes <- ggplot2::ggplot_build(plot(x, panel = panel))$data
    expect_true(15 %in% unlist(lapply(shapes, `[[`, "shape")))
  }
  few <- gg_learning(.fake_learning(c(0.5, 1)), top_k = 3, min_events = 500)
  expect_equal(as.character(.learning_flag_label(attr(few, "stability")))[2],
               "whole cohort (expected), few events")
  built <- ggplot2::ggplot_build(plot(few))$data[[2]]
  expect_true(0 %in% built$shape)
})

test_that("the error panel's mean keeps its fraction's marker", {
  # One replicate: the mean sits exactly on the refit, so a plain mean
  # circle drawn on top would hide the bootstrap square.
  x <- gg_learning(.fake_learning(c(0.5, 1), n_rep = 1L), top_k = 3)
  built <- ggplot2::ggplot_build(plot(x, panel = "error"))$data
  expect_length(built, 3L)
  refits <- built[[1L]]
  means <- built[[3L]]
  expect_equal(means$x, refits$x)
  expect_equal(means$y, refits$y)
  expect_equal(means$shape, refits$shape)
  expect_equal(means$shape, c(16, 15))
  # The open marker survives as well.
  few <- gg_learning(.fake_learning(c(0.5, 1), n_rep = 1L), top_k = 3,
                     min_events = 500)
  means <- ggplot2::ggplot_build(plot(few, panel = "error"))$data[[3L]]
  expect_equal(means$shape, c(1, 0))
  # Several replicates: one mean per fraction, still in order of events.
  means <- ggplot2::ggplot_build(plot(gg_learning(.fake_learning(),
                                                  top_k = 3),
                                      panel = "error"))$data[[3L]]
  expect_equal(nrow(means), 4L)
  expect_false(is.unsorted(means$x))
})

test_that("stability is NA with a single replicate", {
  st <- attr(gg_learning(.fake_learning(0.5, n_rep = 1L)), "stability")
  expect_true(is.na(st$jaccard))
  expect_true(is.na(st$kendall))
})

test_that("gg_learning rejects bad input with clear errors", {
  expect_error(gg_learning(list(1, 2)), "subsample\\(\\) results")
  expect_error(gg_learning(.fake_learning(0.5, performance = FALSE)),
               "performance = TRUE")
  expect_error(gg_learning(.fake_learning(c(0.5, 0.5))), "only once")
  mixed <- c(.fake_learning(0.5), .fake_learning(0.25, family = "regr"))
  expect_error(gg_learning(mixed), "same forest")
  expect_error(gg_learning(.fake_learning(0.5), top_k = 0), "top_k")
  expect_error(gg_learning(.fake_learning(0.5), top_k = 2.5), "top_k")
  expect_error(gg_learning(.fake_learning(0.5), min_events = -1),
               "min_events")
  odd <- .fake_learning(0.5)
  odd[[1]]$rf$family <- "surv-CR"
  expect_error(gg_learning(odd), "not family 'surv-CR'")
  rf <- structure(list(), class = "randomForest")
  expect_error(gg_learning(rf), "no subsample")
})

test_that("plot.gg_learning returns a ggplot for each panel", {
  x <- gg_learning(.fake_learning(), top_k = 3)
  p1 <- plot(x)
  p2 <- plot(x, panel = "error")
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_equal(p1$labels$y, "Agreement of top-3 VIMP rankings")
  expect_equal(p2$labels$y, "OOB error (1 - C-index)")
  expect_equal(p1$labels$x, "Events")
  expect_error(plot(x, panel = "bogus"), "should be one of")
  expect_silent(ggplot2::ggplot_build(p1))
  expect_error(plot.gg_learning(data.frame()), "requires a 'gg_learning'")
})

test_that("plot.gg_learning labels the x axis by family", {
  rg <- gg_learning(.fake_learning(0.5, family = "regr"))
  cl <- gg_learning(.fake_learning(0.5, family = "class"))
  expect_equal(plot(rg)$labels$x, "Observations")
  expect_equal(plot(cl, panel = "error")$labels$x,
               "Cases in the smallest class")
  expect_equal(plot(cl, panel = "error")$labels$y, "OOB error rate")
})

test_that("summary and print describe each fraction", {
  x <- gg_learning(.fake_learning(), top_k = 3)
  s <- summary(x)
  expect_equal(nrow(s), 4L)
  expect_equal(s$error_mean[1], mean(x$error[x$fraction == 0.25]))
  expect_output(print(x), "gg_learning")
  expect_output(print(x), "top_k: 3")
})

# The fixtures above fake the subsample() shape; these pin it against the
# installed randomForestSRC, family by family.
test_that("gg_learning reads real subsample() output in every family", {
  skip_on_cran()
  set.seed(20261116L)
  old <- options(rf.cores = 1L)
  on.exit(options(old), add = TRUE)
  data(pbc, package = "randomForestSRC", envir = environment())
  pbc <- stats::na.omit(pbc)[1:150, ]
  fits <- list(
    surv = randomForestSRC::rfsrc(Surv(days, status) ~ ., pbc, ntree = 25,
                                  importance = "permute", seed = -1),
    class = randomForestSRC::rfsrc(Species ~ ., iris, ntree = 25,
                                   importance = "permute", seed = -1),
    regr = randomForestSRC::rfsrc(mpg ~ ., mtcars, ntree = 25,
                                  importance = "permute", seed = -1)
  )
  for (fam in names(fits)) {
    ss <- list(
      randomForestSRC::subsample(fits[[fam]], B = 3, subratio = 0.5,
                                 performance = TRUE, verbose = FALSE),
      randomForestSRC::subsample(fits[[fam]], B = 3, bootstrap = TRUE,
                                 performance = TRUE, verbose = FALSE)
    )
    x <- gg_learning(ss, top_k = 3, min_events = 0)
    expect_equal(unique(x$fraction), c(0.5, 1), info = fam)
    expect_true(all(is.finite(x$error)), info = fam)
    expect_equal(attr(x, "provenance")$family, fam)
    expect_equal(attr(x, "stability")$B, c(3L, 3L), info = fam)
  }
  no_perf <- randomForestSRC::subsample(fits$regr, B = 2, subratio = 0.5,
                                        verbose = FALSE)
  expect_error(gg_learning(no_perf), "performance = TRUE")

  # subratio = 1 refits the same cases; only the double bootstrap may stand
  # for the whole cohort.
  whole <- randomForestSRC::subsample(fits$regr, B = 2, subratio = 1,
                                      performance = TRUE, verbose = FALSE)
  expect_error(gg_learning(whole), "bootstrap = TRUE")

  # The reported counts are the ones a stratified draw takes, checked against
  # randomForestSRC's own sampler on an odd event count.
  status <- fits$surv$yvar[[2L]]
  for (f in c(0.25, 0.5)) {
    pt <- randomForestSRC:::make.strat.sample(status, f)
    s <- randomForestSRC::subsample(fits$surv, B = 2, subratio = f,
                                    performance = TRUE, verbose = FALSE)
    st <- attr(gg_learning(s, min_events = 0), "stability")
    expect_equal(st$n, length(pt), info = f)
    expect_equal(st$events, sum(status[pt] == 1), info = f)
  }
  # Unstratified draws are accepted and reported at the stratified counts,
  # as documented: the result keeps no record of what was drawn.
  un <- randomForestSRC::subsample(fits$surv, B = 2, subratio = 0.5,
                                   stratify = FALSE, performance = TRUE,
                                   verbose = FALSE)
  un_st <- attr(gg_learning(un, min_events = 0, stratified = FALSE),
                "stability")
  expect_equal(un_st$events, as.integer(sum(status == 1) * 0.5))
  expect_true(is.na(un_st$low_events))

  # Two real forests with the same dimensions but different data.
  shuffled <- mtcars
  shuffled$mpg <- rev(shuffled$mpg)
  other <- randomForestSRC::rfsrc(mpg ~ ., shuffled, ntree = 25,
                                  importance = "permute", seed = -1)
  pair <- list(
    randomForestSRC::subsample(fits$regr, B = 2, subratio = 0.5,
                               performance = TRUE, verbose = FALSE),
    randomForestSRC::subsample(other, B = 2, subratio = 0.25,
                               performance = TRUE, verbose = FALSE)
  )
  expect_error(gg_learning(pair), "same forest")

  # learning_subsample() records the draw.
  set.seed(20261116L)
  ls <- learning_subsample(fits$surv, fractions = c(0.5, 0.25),
                           replicates = 2)
  expect_length(ls, 3L)
  expect_equal(vapply(ls[1:2], function(s) s$subratio, numeric(1)),
               c(0.25, 0.5))
  expect_s3_class(ls[[3]], "bootsample")
  expect_true(all(vapply(ls, attr, logical(1), which = "stratify")))
  st <- attr(gg_learning(ls, min_events = 0), "stability")
  expect_equal(st$counts, c("exact", "exact", "expected"))
  ls_un <- learning_subsample(fits$surv, fractions = 0.5, replicates = 2,
                              bootstrap = FALSE, stratify = FALSE)
  expect_length(ls_un, 1L)
  st <- attr(gg_learning(ls_un, min_events = 0), "stability")
  expect_equal(st$counts, "expected")
  expect_true(is.na(st$low_events))

  # A forest grown without VIMP (the default): subsample() recomputes it on
  # every call, so each result's copy of the forest differs in timings and
  # derived output, and must still count as one forest.
  plain <- randomForestSRC::rfsrc(mpg ~ ., mtcars, ntree = 15, seed = -1)
  set.seed(20261116L)
  ls_plain <- learning_subsample(plain, fractions = c(0.5, 0.7),
                                 replicates = 2)
  x <- gg_learning(ls_plain, top_k = 3, min_events = 0)
  expect_equal(attr(x, "stability")$fraction, c(0.5, 0.7, 1))
  # ... while a different parent grown the same way is still refused.
  plain2 <- randomForestSRC::rfsrc(mpg ~ ., mtcars, ntree = 15, seed = -2)
  mixed <- c(ls_plain[1L], learning_subsample(plain2, fractions = 0.25,
                                              replicates = 2,
                                              bootstrap = FALSE))
  expect_error(gg_learning(mixed), "same forest")

  # Same data, one setting apart.
  coarse <- randomForestSRC::rfsrc(mpg ~ ., mtcars, ntree = 25, nsplit = 1,
                                   importance = "permute", seed = -1)
  pair <- list(
    randomForestSRC::subsample(fits$regr, B = 2, subratio = 0.5,
                               performance = TRUE, verbose = FALSE),
    randomForestSRC::subsample(coarse, B = 2, subratio = 0.25,
                               performance = TRUE, verbose = FALSE)
  )
  expect_error(gg_learning(pair), "same forest")
})

test_that("learning_subsample rejects bad arguments before fitting", {
  fit <- structure(list(), class = "rfsrc")
  expect_error(learning_subsample(list()), "randomForestSRC forest")
  expect_error(learning_subsample(fit, fractions = 1), "less than 1")
  expect_error(learning_subsample(fit, fractions = c(0.5, NA)), "fractions")
  expect_error(learning_subsample(fit, replicates = 0), "'replicates'")
  expect_error(learning_subsample(fit, bootstrap = NA), "'bootstrap'")
  expect_error(learning_subsample(fit, stratify = "yes"), "'stratify'")
  expect_error(learning_subsample(fit, B = 5), "sets 'B' itself")
  expect_error(learning_subsample(fit, subratio = 0.5, verbose = TRUE),
               "sets 'subratio', 'verbose' itself")
})
