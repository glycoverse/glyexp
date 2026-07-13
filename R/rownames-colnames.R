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
#' @template deprecated-experiment
#' @export
#' @method dimnames glyexp_experiment
dimnames.glyexp_experiment <- function(x, ...) {
  .deprecate_experiment_api("dimnames.glyexp_experiment()")
  stopifnot(.is_experiment(x))
  dimnames(x$expr_mat)
}
