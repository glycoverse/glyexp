#' Get Samples or Variables of an Experiment
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
#' @export
samples <- function(exp) {
  stopifnot(is_experiment(exp))
  colnames(exp$expr_mat)
}


#' @rdname samples
#' @export
variables <- function(exp) {
  stopifnot(is_experiment(exp))
  rownames(exp$expr_mat)
}


#' Get Number of Samples or Variables of an Experiment
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
#' @export
n_samples <- function(exp) {
  stopifnot(is_experiment(exp))
  ncol(exp$expr_mat)
}


#' @rdname n_samples
#' @export
n_variables <- function(exp) {
  stopifnot(is_experiment(exp))
  nrow(exp$expr_mat)
}
