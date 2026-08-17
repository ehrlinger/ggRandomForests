# autoplot() and plot() must produce the same plot for every gg_* class.
#
# All 19 autoplot methods are thin delegators: autoplot.gg_x() calls plot() or
# plot.gg_x() and returns the result. That makes the failure mode a drift one.
# Someone adds an argument, a default or a layer to plot.gg_x and does not
# carry it through the delegator, or replaces a delegator with a second
# implementation that starts out identical and then diverges. Nothing in the
# suite noticed, because the existing autoplot tests only assert
# expect_s3_class(p, "ggplot"), which stays true no matter how far the two
# drift apart.
#
# A vdiffr baseline per autoplot method would also catch this, but it would
# mean 19 more SVGs rendering the same plots the plot() baselines already
# cover, and a visual diff answers "these images differ" rather than "the
# delegation broke". Comparing the ggplot objects is cheaper and names the
# actual defect. Audited 2026-08-17: all 49 existing baselines exercise
# plot(); none exercise autoplot().
#
# DO NOT rewrite same_plot() as expect_equal(autoplot(x), plot(x)).
#
# On ggplot2 4.x a ggplot is an S7 object, all.equal() has no S7 method, and
# the fallback compares nothing useful: all.equal() returns TRUE for two plots
# with different titles. That version of this test passed while an
# autoplot.gg_vimp() carrying an extra labs(caption = ...) was installed, which
# is how the problem was found. Verified on ggplot2 4.0.3.
#
# identical() is no use either: it is FALSE even for plot(g) against plot(g),
# because each carries its own plot environment.
#
# So compare the three things that actually characterise the plot, all of which
# are ordinary R objects: the labels, the built layer data, and the geoms.

# Some plot methods are not deterministic at BUILD time: plot.gg_rfsrc and
# plot.gg_shap jitter or beeswarm their points, and the random draw happens
# inside ggplot_build(), not when the plot object is constructed. Two builds of
# the very same object therefore differ. Seeding before each build is what
# makes this a comparison of delegation rather than of jitter.
# ggplot_build()$data is post-stat but PRE-COORD, and $labels carries nothing
# about coords, themes, scales or facets. Comparing only those two plus the
# geoms therefore treated several whole classes of divergence as a match.
# Measured on ggplot2 4.0.3 against plot(gg_error(rf)), each of these is
# invisible to labels + built data + geoms alone:
#
#   + coord_flip()      transposes the plot
#   + theme_bw()        restyles it
#   + scale_fill_grey() recolours it
#   + facet_wrap(...)   splits it into panels
#
# Note that a coord mutation on gg_vimp specifically is NOT detectable, and
# should not be: plot.gg_vimp already applies coord_flip(), so adding another
# replaces CoordFlip with CoordFlip and the plot really is unchanged. The
# self-test below uses gg_error, whose plot is CoordCartesian, for exactly that
# reason.
# Everything about the plot that is an ordinary R object and so can be compared
# with identical(). Kept separate from same_plot() because chaining these as one
# && expression pushed cyclocomp past this repo's limit of 20, and the house
# rule is to extract a helper rather than raise the cap.
plot_shape <- function(p) {
  list(
    labels = p$labels,
    geoms  = vapply(p$layers, function(l) class(l$geom)[1], character(1)),
    coord  = class(p$coordinates)[1],
    facet  = class(p$facet)[1],
    scales = vapply(p$scales$scales, function(s) class(s)[1], character(1)),
    theme  = p$theme
  )
}

# Seeded because the draw happens here, not at construction.
built_data <- function(p, seed) {
  set.seed(seed)
  ggplot2::ggplot_build(p)$data
}

same_plot <- function(a, b, seed = 101L) {
  identical(plot_shape(a), plot_shape(b)) &&
    isTRUE(all.equal(built_data(a, seed), built_data(b, seed)))
}

test_that("autoplot() equals plot() for every cheaply constructible gg_* class", {
  skip_on_cran()
  skip_if_not_installed("randomForestSRC")
  skip_if_not_installed("randomForest")
  set.seed(20260817L)

  rc <- randomForestSRC::rfsrc(
    Species ~ ., iris,
    ntree = 30, importance = TRUE, tree.err = TRUE
  )
  rr <- randomForestSRC::rfsrc(
    mpg ~ ., mtcars,
    ntree = 30, importance = TRUE, tree.err = TRUE
  )
  rs <- randomForestSRC::rfsrc(
    Surv(time, status) ~ ., survival::veteran,
    ntree = 30, importance = TRUE, tree.err = TRUE
  )
  rf <- randomForest::randomForest(Species ~ ., iris, ntree = 30, importance = TRUE)

  objects <- list(
    "gg_vimp (rfsrc classification)"   = gg_vimp(rc),
    "gg_vimp (randomForest)"           = gg_vimp(rf),
    "gg_error (classification)"        = gg_error(rc),
    "gg_error (survival)"              = gg_error(rs),
    "gg_rfsrc (classification)"        = gg_rfsrc(rc),
    "gg_rfsrc (regression)"            = gg_rfsrc(rr),
    "gg_rfsrc (survival)"              = gg_rfsrc(rs),
    "gg_roc (classification)"          = gg_roc(rc, which_outcome = 1),
    "gg_variable (regression)"         = gg_variable(rr),
    "gg_brier (survival)"              = gg_brier(rs),
    "gg_shap (regression)"             = gg_shap(rr),
    "gg_survival"                      = gg_survival(
      interval = "time", censor = "status", by = "trt", data = survival::veteran
    )
  )

  # Some plot methods are not deterministic: plot.gg_rfsrc and plot.gg_shap
  # jitter or beeswarm their points, so two renders of the SAME object differ
  # in built data. Reseed immediately before each render so both draw the same
  # random numbers and the comparison measures delegation, not jitter.
  for (label in names(objects)) {
    obj <- objects[[label]]
    set.seed(101L)
    rendered_autoplot <- ggplot2::autoplot(obj)
    set.seed(101L)
    rendered_plot <- plot(obj)
    expect_true(
      same_plot(rendered_autoplot, rendered_plot),
      label = paste0("autoplot() matches plot() for ", label)
    )
  }

  # A guard on the guard: if a future refactor makes these objects fail to
  # build, the loop above would silently assert nothing at all.
  expect_length(objects, 12L)
})

test_that("same_plot() can actually tell two plots apart", {
  # Without this, the test above is one ggplot2 release away from passing
  # vacuously again, exactly as the expect_equal() version already did. This
  # pins the discriminating power itself rather than trusting it.
  skip_if_not_installed("randomForestSRC")
  set.seed(20260817L)
  rf <- randomForestSRC::rfsrc(
    Species ~ ., iris,
    ntree = 20, importance = TRUE, tree.err = TRUE
  )

  # gg_error, not gg_vimp: plot.gg_vimp already applies coord_flip(), so
  # adding another is genuinely a no-op there and would make the coord case
  # untestable. plot.gg_error is CoordCartesian.
  base <- plot(gg_error(rf))

  expect_true(same_plot(base, base))
  expect_false(same_plot(base, base + ggplot2::labs(caption = "drifted")))
  expect_false(same_plot(base, base + ggplot2::ggtitle("different")))
  expect_false(same_plot(base, base + ggplot2::coord_flip()))
  expect_false(same_plot(base, base + ggplot2::theme_bw()))
  expect_false(same_plot(base, base + ggplot2::scale_colour_grey()))
})
