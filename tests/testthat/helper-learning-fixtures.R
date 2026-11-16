# Deterministic stand-ins for randomForestSRC::subsample() results, shaped as
# rfsrc 3.10.0 returns them: replicate VIMP tables in `vmpS` (or `vmpB` for the
# double bootstrap), one data frame per outcome, with the OOB error appended
# as an "err" row when performance = TRUE. No forest is grown, so the vdiffr
# baselines do not move with randomForestSRC.
.fake_learning_one <- function(fraction, n_rep = 3L, performance = TRUE,
                               family = "surv", events = 100L) {
  xvars <- paste0("x", 1:6)
  rf <- list(
    family = family, n = 400L, ntree = 100L, xvar.names = xvars,
    yvar = if (family == "surv") {
      data.frame(time = seq_len(400),
                 status = rep(c(1L, 0L), c(events, 400L - events)))
    } else if (family == "class") {
      factor(rep(c("a", "b", "b"), length.out = 400))
    } else {
      seq_len(400) / 400
    }
  )
  base <- c(0.30, 0.25, 0.20, 0.06, 0.05, 0.04)
  reps <- lapply(seq_len(n_rep), function(b) {
    # Noise shrinks as the fraction grows, so the ranking settles.
    v <- base + (1 - fraction) * 0.12 * sin(seq_along(base) * b * 1.7)
    if (performance) v <- c(v, 0.40 - 0.10 * fraction + 0.01 * b)
    tab <- data.frame(all = v)
    rownames(tab) <- c(xvars, if (performance) "err")
    list(y = tab)
  })
  boot <- fraction == 1
  out <- list(rf = rf, vmp = NULL, subratio = if (boot) NULL else fraction,
              performance.only = FALSE)
  out[[if (boot) "vmpB" else "vmpS"]] <- reps
  class(out) <- c("rfsrc", "grow", family, if (boot) "bootsample" else "subsample")
  out
}

.fake_learning <- function(fractions = c(0.25, 0.5, 0.75, 1), ...) {
  lapply(fractions, .fake_learning_one, ...)
}
