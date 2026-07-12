#' Dimname for experiment
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This method was deprecated with `experiment()`. Use `dimnames()` on a
#' `SummarizedExperiment` instead.
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
#' @keywords internal
#' @export
#' @method dimnames glyexp_experiment
dimnames.glyexp_experiment <- function(x, ...) {
  .deprecate_experiment("dimnames.glyexp_experiment()", "dimnames()")
  stopifnot(.is_experiment(x))
  dimnames(x$expr_mat)
}
