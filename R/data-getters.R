#' Get the expression matrix of an experiment
#'
#' A `matrix` of expression values with samples as columns and variables as rows.
#'
#' @param exp An [experiment()].
#' @returns A matrix of expression values.
#' @export
get_expr_mat <- function(exp) {
  stopifnot(class(exp) == "glyexp_experiment")
  exp$expr_mat
}


#' Get the sample information of an experiment
#'
#' A `tibble` of sample information, with the first column being "sample".
#'
#' @param exp An [experiment()].
#' @returns A tibble of sample information.
#' @export
get_sample_info <- function(exp) {
  stopifnot(class(exp) == "glyexp_experiment")
  exp$sample_info
}


#' Get the variable information of an experiment
#'
#' A `tibble` of variable information, with the first column being "variable".
#'
#' @param exp An [experiment()].
#' @returns A tibble of variable information.
#' @export
get_var_info <- function(exp) {
  stopifnot(class(exp) == "glyexp_experiment")
  exp$var_info
}
