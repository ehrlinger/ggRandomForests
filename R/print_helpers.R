####**********************************************************************
####  Internal helpers shared by print.gg_* and summary.gg_* methods.
####
####  Every gg_* extractor calls .set_provenance() on the object it
####  returns, attaching a small list under attr(x, "provenance") that
####  captures the source package, forest family, ntree, and predictor
####  set. The print/summary methods consume that attribute to render a
####  uniform header.
####
####  Provenance is best-effort: missing fields are NA. Methods must
####  tolerate the attribute being absent (older saved objects).
####**********************************************************************

# Build the provenance list from a forest object.
.gg_provenance <- function(object) {
  if (inherits(object, "rfsrc")) {
    return(list(
      source     = "randomForestSRC",
      family     = object$family %||% NA_character_,
      ntree      = object$ntree %||% NA_integer_,
      n          = object$n %||% NA_integer_,
      xvar.names = object$xvar.names %||% character(0)
    ))
  }
  if (inherits(object, "randomForest")) {
    return(list(
      source     = "randomForest",
      family     = object$type %||% NA_character_,
      ntree      = object$ntree %||% NA_integer_,
      n          = if (!is.null(object$y)) length(object$y) else NA_integer_,
      xvar.names = if (!is.null(object$importance)) {
        rownames(object$importance)
      } else {
        character(0)
      }
    ))
  }
  # Fallback (e.g. gg_survival.default fed a data.frame).
  list(
    source     = NA_character_,
    family     = NA_character_,
    ntree      = NA_integer_,
    n          = NA_integer_,
    xvar.names = character(0)
  )
}

# Attach provenance to a gg_* object and return it unchanged.
.set_provenance <- function(x, object) {
  attr(x, "provenance") <- .gg_provenance(object)
  x
}

# Format the standard one-line header used by print.gg_* methods.
# Returns a character scalar.
.gg_header <- function(x, class_label) {
  prov <- attr(x, "provenance")
  if (is.null(prov)) {
    return(sprintf("<%s>", class_label))
  }
  src    <- prov$source    %||% NA
  family <- prov$family    %||% NA
  ntree  <- prov$ntree     %||% NA
  n      <- prov$n         %||% NA

  bits <- c(
    if (!is.na(src))    sprintf("from %s", src),
    if (!is.na(family)) sprintf("family: %s", family),
    if (!is.na(ntree))  sprintf("ntree: %s", ntree),
    if (!is.na(n))      sprintf("n: %s", n)
  )
  sprintf("<%s>  %s", class_label, paste(bits, collapse = "  |  "))
}

# Null-coalescing helper (avoid rlang dependency).
`%||%` <- function(a, b) if (is.null(a)) b else a
