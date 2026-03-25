#' Display the ggRandomForests NEWS file
#'
#' Opens the package NEWS file in the system pager so users can read the
#' version history and change log without leaving their R session.
#'
#' @param ... Currently unused; reserved for future arguments.
#'
#' @return Called for its side-effect of opening the NEWS file in the system
#'   pager (\code{file.show}).  Returns \code{invisible(NULL)}.
#'
#' @keywords internal
ggrandomforests.news <- function(...) {
  newsfile <- file.path(system.file(package="ggRandomForests"), "NEWS")
  file.show(newsfile)
}