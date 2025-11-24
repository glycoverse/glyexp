#' Dimname for experiment
#'
#' The dimnames method for [experiment()] objects are
#' the dimnames of their expression matrix.
#'
#' @param x An [experiment()].
#' @param ... Ignored.
#'
#' @return A list with the dimnames of the expression matrix.
#'
#' @examples
#' dimnames(real_experiment)
#'
#' @export
#' @method dimnames glyexp_experiment
dimnames.glyexp_experiment <- function(x, ...) {
  stopifnot(is_experiment(x))
  dimnames(x$expr_mat)
}
