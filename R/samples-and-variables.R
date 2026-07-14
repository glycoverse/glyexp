#' Get Samples or Variables of an Experiment
#'
#' Getting the names of samples or variables of an [experiment()].
#' Syntax sugar for `colnames(exp$expr_mat)` and `rownames(exp$expr_mat)`.
#'
#' @param exp An [experiment()].
#'
#' @return A character vector of sample or variable names.
#'
#' @template deprecated-experiment
#' @export
samples <- function(exp) {
  .deprecate_experiment_api("samples()")
  stopifnot(.is_experiment(exp))
  colnames(exp$expr_mat)
}


#' @rdname samples
#' @export
variables <- function(exp) {
  .deprecate_experiment_api("variables()")
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
#' @template deprecated-experiment
#' @export
n_samples <- function(exp) {
  .deprecate_experiment_api("n_samples()")
  stopifnot(.is_experiment(exp))
  ncol(exp$expr_mat)
}


#' @rdname n_samples
#' @export
n_variables <- function(exp) {
  .deprecate_experiment_api("n_variables()")
  stopifnot(.is_experiment(exp))
  nrow(exp$expr_mat)
}
