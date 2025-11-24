#' Dimensions of an experiment
#'
#' Retrieve the dimensions of an experiment object,
#' i.e. the number of variables and samples.
#'
#' @param x An experiment object.
#' @param value Ignored.
#'
#' @return A vector with two elements: the number of variables and the number of samples.
#' @examples
#' dim(real_experiment)
#' @export
dim.glyexp_experiment <- function(x) {
  dim(x$expr_mat)
}


#' @rdname dim.glyexp_experiment
#' @export
`dim<-.glyexp_experiment` <- function(x, value) {
  cli::cli_abort("Dimensions of an experiment could not be set manually.")
}
