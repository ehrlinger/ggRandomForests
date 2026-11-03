# gg_* wrappers on a genuine time-dependent fit: several start-stop rows per
# case and internal gaps. See .rhf_tvc() in helper-rhf-fixtures.R.

# Grid points are classified with a tolerance, because rhf() rescales times
# internally and a grid point that sits on a supplied stop time does not
# compare equal to it after the round trip.
.tvc_path <- function(d, id, time) {
  r <- d[d$id == id, c("start", "stop")]
  eps <- 1e-8 * max(time)
  list(
    inside = vapply(time, function(t) any(t > r$start + eps & t < r$stop - eps),
                    logical(1)),
    gap = time > r$stop[1] + eps & time < r$start[2] - eps,
    after = time > max(r$stop) + eps
  )
}

test_that("the tvc fixture really has multi-row cases with gaps", {
  set.seed(20261103L)
  d <- .rhf_tvc_data()
  o <- .rhf_tvc()
  expect_gt(max(table(d$id)), 1L)
  expect_true(all(tapply(d$event, d$id, sum) <= 1))
  gap_points <- vapply(attr(d, "gap_ids"), function(i) {
    sum(.tvc_path(d, i, o$time.interest)$gap)
  }, numeric(1))
  expect_true(all(gap_points > 0))
})

test_that("gg_rhf keeps a multi-row case's hazard on its supplied path", {
  set.seed(20261103L)
  d  <- .rhf_tvc_data()
  o  <- .rhf_tvc()
  gg <- gg_rhf(o)
  expect_s3_class(gg, "gg_rhf")
  expect_equal(nrow(gg), nrow(o$hazard.oob) * length(o$time.interest))
  expect_setequal(unique(gg$id), unique(d$id))
  expect_identical(is.na(gg$hazard), as.vector(is.na(o$hazard.oob)))
  expect_identical(is.na(gg$chf), as.vector(is.na(o$chf.oob)))
  for (i in attr(d, "gap_ids")) {
    g <- gg[gg$id == i, ]
    g <- g[order(g$time), ]
    p <- .tvc_path(d, i, g$time)
    expect_true(all(!is.na(g$hazard[p$inside])), info = paste("id", i))
    expect_true(all(is.na(g$hazard[p$gap | p$after])), info = paste("id", i))
  }
})

test_that("gg_rhf keeps a multi-row case's chf flat across gaps", {
  set.seed(20261103L)
  d  <- .rhf_tvc_data()
  gg <- gg_rhf(.rhf_tvc())
  for (i in attr(d, "gap_ids")) {
    g <- gg[gg$id == i, ]
    g <- g[order(g$time), ]
    p <- .tvc_path(d, i, g$time)
    # ?rhf documents the cumulative hazard as flat through a gap, but
    # randomForestRHF 2.1.0 returns NA from a case's first gap to the end of its
    # follow-up. ggRF passes chf through, so accept either rule. Flat means
    # carried forward: finite values in a gap must all equal the last value
    # before it, so a chf that jumps on entering the gap
    # fails here, and one that falls across the gap fails the diff() below.
    in_gap <- g$chf[p$gap]
    if (!all(is.na(in_gap))) {
      before <- g$chf[which(p$gap)[1] - 1L]
      expect_true(all(abs(in_gap - before) < 1e-12), info = paste("id", i))
    }
    expect_true(all(is.na(g$chf[p$after])), info = paste("id", i))
    chf <- g$chf[!is.na(g$chf)]
    expect_true(all(diff(chf) >= -1e-12), info = paste("id", i))
  }
})

test_that("plot.gg_rhf draws a multi-row case", {
  set.seed(20261103L)
  d  <- .rhf_tvc_data()
  gg <- gg_rhf(.rhf_tvc())
  expect_s3_class(plot(gg, idx = attr(d, "gap_ids")[1:3]), "ggplot")
  expect_s3_class(plot(gg, idx = attr(d, "gap_ids")[1], hazard.only = FALSE),
                  "ggplot")
})

test_that("plot.gg_rhf does not draw a line across a gap", {
  # A line group that has points on both sides of a gap and none inside it is
  # a straight line over time the case was not observed. Each case is plotted
  # alone, so every line group in the built plot belongs to it.
  set.seed(20261103L)
  d  <- .rhf_tvc_data()
  gg <- gg_rhf(.rhf_tvc())
  for (i in attr(d, "gap_ids")) {
    r <- d[d$id == i, c("start", "stop")]
    eps <- 1e-8 * max(gg$time)
    for (haz in c(TRUE, FALSE)) {
      # On 2.1.0 a case whose first stop precedes the first grid point has no
      # finite chf at all, so it has no line to bridge.
      if (!any(is.finite(gg[gg$id == i, if (haz) "hazard" else "chf"]))) next
      built <- ggplot2::ggplot_build(plot(gg, idx = i, hazard.only = haz))
      bridges <- vapply(split(built$data[[1]]$x, built$data[[1]]$group),
                        function(x) {
                          any(x <= r$stop[1] + eps) && any(x >= r$start[2] - eps) &&
                            !any(x > r$stop[1] + eps & x < r$start[2] - eps)
                        }, logical(1))
      expect_false(any(bridges), info = paste("id", i, "hazard.only", haz))
    }
  }
})

test_that("gg_auct runs on a time-dependent fit", {
  set.seed(20261103L)
  o  <- .rhf_tvc()
  gg <- gg_auct(o, marker = "chf")
  expect_s3_class(gg, "gg_auct")
  expect_true(any(is.finite(gg$auc)))
  expect_s3_class(plot(gg), "ggplot")
})

test_that("gg_rhf_importance runs on a time-dependent fit", {
  set.seed(20261103L)
  o <- .rhf_tvc()
  cache <- randomForestRHF::varpro.cache.rhf(o, max.rules.tree = 30L,
                                             max.tree = 12L)
  idx <- unique(pmax(1L, round(seq(1L, cache$K, length.out = 4L))))
  gg <- gg_rhf_importance(o, cache = cache, time.index = idx)
  expect_s3_class(gg, "gg_rhf_importance")
  expect_setequal(as.character(unique(gg$variable)), o$xvar.names)
  expect_s3_class(plot(gg), "ggplot")
})

test_that("rhf() still refuses recurrent events", {
  # A canary, not a ggRandomForests contract. randomForestRHF 2.1.0 rejects
  # more than one event per case, and an event on a case's non-final row. When
  # this starts failing, upstream has enabled recurrent-event fitting, and the
  # gg_* wrappers need a look before anyone feeds them recurrent data.
  skip_on_cran()
  skip_if_not_installed("randomForestRHF")
  set.seed(20261103L)
  d <- data.frame(id = rep(1:30, each = 2), start = rep(c(0, 1), 30),
                  stop = rep(c(1, 2), 30), event = 1L, x = rnorm(60))
  expect_error(
    randomForestRHF::rhf("Surv(id, start, stop, event) ~ x", d, ntree = 5),
    "[Rr]ecurrent"
  )
  d$event <- rep(c(1L, 0L), 30)
  expect_error(
    randomForestRHF::rhf("Surv(id, start, stop, event) ~ x", d, ntree = 5),
    "[Rr]ecurrent"
  )
})
