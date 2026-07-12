#' Dimensions of an experiment
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These methods were deprecated with `experiment()`. Use the corresponding
#' base dimension operations on a `SummarizedExperiment` instead.
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
#' @keywords internal
#' @export
dim.glyexp_experiment <- function(x) {
  .deprecate_experiment("dim.glyexp_experiment()", "dim()")
  dim(x$expr_mat)
}


#' @rdname dim.glyexp_experiment
#' @export
`dim<-.glyexp_experiment` <- function(x, value) {
  .deprecate_experiment(
    "experiment()",
    details = "Use base dimension operations on a SummarizedExperiment instead."
  )
  cli::cli_abort("Dimensions of an experiment could not be set manually.")
}
