#' Get Samples or Variables of an Experiment
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These helpers were deprecated with `experiment()`. Use `colnames()`,
#' `rownames()`, `ncol()`, and `nrow()` on a `SummarizedExperiment` instead.
#'
#' Getting the names of samples or variables of an [experiment()].
#' Syntax sugar for `colnames(exp$expr_mat)` and `rownames(exp$expr_mat)`.
#'
#' @param exp An [experiment()].
#'
#' @return A character vector of sample or variable names.
#'
#' @examples
#' exp <- toy_experiment
#' samples(exp)
#' variables(exp)
#'
#' @keywords internal
#' @export
samples <- function(exp) {
  .deprecate_experiment("samples()", "colnames()")
  stopifnot(.is_experiment(exp))
  colnames(exp$expr_mat)
}


#' @rdname samples
#' @export
variables <- function(exp) {
  .deprecate_experiment("variables()", "rownames()")
  stopifnot(.is_experiment(exp))
  rownames(exp$expr_mat)
}


#' Get number of samples or variables of an experiment
#'
#' Getting the number of samples or variables of an [experiment()].
#' Syntax sugar for `ncol(exp$expr_mat)` and `nrow(exp$expr_mat)`.
#'
#' @param exp An [experiment()].
#'
#' @return An integer with the number of samples or variables.
#'
#' @examples
#' exp <- toy_experiment
#' n_samples(exp)
#' n_variables(exp)
#'
#' @keywords internal
#' @export
n_samples <- function(exp) {
  .deprecate_experiment("n_samples()", "ncol()")
  stopifnot(.is_experiment(exp))
  ncol(exp$expr_mat)
}


#' @rdname n_samples
#' @export
n_variables <- function(exp) {
  .deprecate_experiment("n_variables()", "nrow()")
  stopifnot(.is_experiment(exp))
  nrow(exp$expr_mat)
}
