#' Get the Expression Matrix of an [experiment()]
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


#' Get the Sample Information of an [experiment()]
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


#' Get the Variable Information of an [experiment()]
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


#' Get the meta data of an [experiment()]
#'
#' Meta data is a named list of arbitrary data about the experiment,
#' e.g. "structure_type", "glycan_type", and "data_type".
#'
#' @param exp An [experiment()].
#' @returns The meta data as a named list.
#' @export
get_meta_data <- function(exp) {
  stopifnot(class(exp) == "glyexp_experiment")
  exp$meta_data
}
