#' Dimensions of an experiment
#'
#' Retrieve the dimensions of an experiment object,
#' i.e. the number of variables and samples.
#'
#' @param x An experiment object.
#' @param value Ignored.
#'
#' @return A vector with two elements: the number of variables and the number of samples.
#' @template deprecated-experiment
#' @export
dim.glyexp_experiment <- function(x) {
  .deprecate_experiment_api("dim.glyexp_experiment()")
  dim(x$expr_mat)
}


#' @rdname dim.glyexp_experiment
#' @export
`dim<-.glyexp_experiment` <- function(x, value) {
  .deprecate_experiment_api(I("Using `dim<-` on a glyexp_experiment object"))
  cli::cli_abort("Dimensions of an experiment could not be set manually.")
}
