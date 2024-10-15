#' Dimname for Experiment
#'
#' The dimnames method for [experiment()] objects are
#' the dimnames of their expression matrix.
#'
#' @param x An [experiment()].
#' @param ... Ignored.
#'
#' @return A list with the dimnames of the expression matrix.
#'
#' @export
#' @method dimnames glyexp_experiment
dimnames.glyexp_experiment <- function(x, ...) {
  stopifnot(is_experiment(x))
  dimnames(x$expr_mat)
}
