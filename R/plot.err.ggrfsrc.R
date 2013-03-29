#' plot.err
#' @export plot.err.ggrfsrc
### error rate plot
plot.err.ggrfsrc <- function(err, ...) {
  opar <- par("cex")
  on.exit(par(opar))
  matplot(1:nrow(err), err,
          xlab = "Number of Trees",
          ylab = "Error Rate",
          type = c("p", "l")[1 + 1 * (nrow(err) > 1)], pch = 16, lty = 1, lwd = 3)
  if (ncol(err) > 1) {
    legend("topright",
           legend = colnames(err), col = 1:ncol(err), lty = 1, lwd = 3)
  }
}

plot.err <- plot.err.ggrfsrc