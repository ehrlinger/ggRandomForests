#' Display the ggRandomForests NEWS file
#'
#' Opens the package change log in the system pager so users can read the
#' version history without leaving their R session. The function reads
#' \code{NEWS.md} from the installed package root (the canonical change log
#' that R also surfaces via \code{utils::news()}). \code{inst/NEWS} was
#' removed in v2.7.1 to eliminate the duplicate-source-of-truth maintenance
#' hole that left the legacy file frozen at v2.4.0; if any installation
#' still ships an \code{inst/NEWS}, this function falls back to it.
#'
#' @param ... Currently unused; reserved for future arguments.
#'
#' @return Called for its side-effect of opening the NEWS file in the system
#'   pager (\code{file.show}). Returns \code{invisible(NULL)}.
#'
#' @keywords internal
ggrandomforests.news <- function(...) {
  newsfile <- system.file("NEWS.md", package = "ggRandomForests")
  if (!nzchar(newsfile)) {
    # Fallback for legacy installs that still bundle inst/NEWS.
    newsfile <- system.file("NEWS", package = "ggRandomForests")
  }
  if (!nzchar(newsfile)) {
    warning("Could not locate a NEWS file for ggRandomForests.",
            call. = FALSE)
    return(invisible(NULL))
  }
  file.show(newsfile)
}
