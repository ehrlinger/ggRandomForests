# Precompute the rare-event simulation in the varpro-small-data vignette.
#
# Synthetic survival data with known signal, at two sizes: 1,900 cases with
# about 60 events, and 12,000 cases with about 150. Each is fitted with
# varPro's defaults and with every variable offered to the forest at three
# settings, and we record how many variables each fit tests and whether the
# true signals come out on top. Only those summaries are saved; no forest is.
#
# Run from the package root (about an hour on one core; the large size is
# most of it):
#   Rscript vignettes/precompute_varpro_rare.R
#
# Produces vignettes/varpro_rare_precomputed.rds. If that file is missing, the
# vignette sources this script with .varpro_rare_fallback set, which runs one
# small-size replicate at one setting only, so the check stays fast.

suppressMessages({
  library(varPro)
  library(survival)
})
options(mc.cores = 1, rf.cores = 1)

fallback <- exists(".varpro_rare_fallback")

# --- Data -----------------------------------------------------------------
# p = 30 predictors. Six carry signal:
#   x1, x2, x3  continuous, log hazard ratio 0.7, 0.5, 0.35 per SD
#   b1, b2      binary, prevalence 10% and 20%, log hazard ratio 1.0, 0.7
#   x4          continuous, log hazard ratio 0.5, correlated 0.7 with n1
# n1 to n24 are noise, in correlated blocks of four (r = 0.5); n5, n10, n15
# and n20 are binary.
rare_truth <- c("x1", "x2", "x3", "b1", "b2", "x4")

rare_sim <- function(n, events, seed) {
  set.seed(seed)
  z <- matrix(stats::rnorm(n * 28), n)
  block <- rep(1:7, each = 4)
  for (k in 1:7) {
    z[, block == k] <- z[, block == k] * sqrt(0.5) + stats::rnorm(n) * sqrt(0.5)
  }
  noise <- as.data.frame(z[, 1:24])
  names(noise) <- paste0("n", 1:24)
  for (j in c(5, 10, 15, 20)) noise[[j]] <- as.integer(noise[[j]] > 1)
  x <- data.frame(x1 = stats::rnorm(n), x2 = stats::rnorm(n),
                  x3 = stats::rnorm(n), b1 = stats::rbinom(n, 1, 0.1),
                  b2 = stats::rbinom(n, 1, 0.2))
  x$x4 <- 0.7 * noise$n1 + sqrt(1 - 0.7^2) * stats::rnorm(n)
  x <- cbind(x, noise)
  lp <- 0.7 * x$x1 + 0.5 * x$x2 + 0.35 * x$x3 + 1.0 * x$b1 + 0.7 * x$b2 +
    0.5 * x$x4
  t_event <- stats::rexp(n, exp(lp))
  t_cens <- stats::runif(n, 0, 5)
  # Scale event times so about `events` of them fall before censoring
  s <- stats::uniroot(function(s) sum(t_event * s < t_cens) - events,
                      c(1e-3, 1e6))$root
  data.frame(time = pmin(t_event * s, t_cens),
             status = as.integer(t_event * s < t_cens), x)
}

# --- Settings ---------------------------------------------------------------
rare_settings <- list(
  defaults   = list(),
  open_all   = list(split.weight = FALSE, sparse = FALSE),
  open_sqrtp = list(split.weight = FALSE, sparse = FALSE, mtry = 6L),
  open_more  = list(split.weight = FALSE, sparse = FALSE, mtry = 6L,
                    ntree = 1000L, max.tree = 1000L, max.rules.tree = 300L)
)

# Share of the scored trees in which a release rule for the variable returned
# an importance value, the coverage gg_varpro() reports. A variable is tested
# when that share reaches 10%, gg_varpro()'s default trim.
rare_coverage <- function(o) {
  r <- o$results
  n_tree <- tapply(r$tree, r$variable, function(t) length(unique(t)))
  out <- stats::setNames(rep(0, length(o$xvar.names)), o$xvar.names)
  out[o$xvar.names[as.integer(names(n_tree))]] <- n_tree
  out / o$max.tree
}

rare_run <- function(n, events, reps, settings) {
  rows <- list()
  for (r in seq_len(reps)) {
    d <- rare_sim(n, events, seed = 1000 + r)
    ev <- sum(d$status)
    for (s in names(settings)) {
      args <- c(list(formula = Surv(time, status) ~ ., data = d,
                     seed = -(1000 + r)), settings[[s]])
      # The extra setting also sizes the outcome forest's leaves for about
      # two events each
      if (s == "open_more") args$nodesize.external <- max(10L, round(2 * n / ev))
      secs <- system.time({
        o <- do.call(varPro::varpro, args)
        imp <- if (is.null(args$max.tree)) varPro::importance(o) else
          varPro::importance(o, max.tree = args$max.tree,
                             max.rules.tree = args$max.rules.tree)
      })[["elapsed"]]
      cv <- rare_coverage(o)
      z <- stats::setNames(rep(NA_real_, ncol(d) - 2), names(d)[-(1:2)])
      z[rownames(imp)] <- imp$z
      top6 <- names(sort(ifelse(is.na(z), -Inf, z), decreasing = TRUE))[1:6]
      rows[[length(rows) + 1L]] <- data.frame(
        n = n, events = ev, rep = r, setting = s, seconds = secs,
        tested = sum(cv >= 0.10), truth_top6 = sum(top6 %in% rare_truth),
        cov_b1 = unname(cv["b1"]), cov_b2 = unname(cv["b2"]),
        z_b1 = unname(z["b1"]), z_x4 = unname(z["x4"]), z_n1 = unname(z["n1"]))
    }
  }
  do.call(rbind, rows)
}

if (fallback) {
  small <- rare_run(1900, 60, reps = 1, rare_settings["open_sqrtp"])
  large <- small[0, ]
} else {
  small <- rare_run(1900, 60, reps = 3, rare_settings)
  large <- rare_run(12000, 150, reps = 1, rare_settings)
}

varpro_rare_objects <- list(
  small = small, large = large, fallback = fallback,
  varpro = format(utils::packageVersion("varPro")),
  rfsrc = format(utils::packageVersion("randomForestSRC"))
)
if (!fallback) {
  saveRDS(varpro_rare_objects,
          file.path("vignettes", "varpro_rare_precomputed.rds"),
          compress = "xz")
}
