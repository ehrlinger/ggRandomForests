test_that("gg_learning tidies replicates, stability and provenance", {
  x <- gg_learning(.fake_learning(c(0.5, 0.25, 1, 0.75)), top_k = 3)
  expect_s3_class(x, c("gg_learning", "data.frame"))
  expect_named(x, c("fraction", "n", "events", "replicate", "error",
                    "low_events", "optimistic"))
  expect_equal(unique(x$fraction), c(0.25, 0.5, 0.75, 1))  # sorted
  expect_equal(nrow(x), 12L)
  # 100 events in 400 cases.
  expect_equal(unique(x$events), c(25, 50, 75, 100))
  expect_equal(x$error[1:3], 0.40 - 0.025 + 0.01 * (1:3))

  st <- attr(x, "stability")
  expect_named(st, c("fraction", "n", "events", "B", "jaccard", "kendall",
                     "low_events", "optimistic"))
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

test_that("gg_learning accepts a single result and caps top_k at p", {
  x <- gg_learning(.fake_learning_one(0.5), top_k = 50)
  expect_equal(attr(x, "provenance")$top_k, 6L)
  expect_equal(nrow(attr(x, "stability")), 1L)
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
})
