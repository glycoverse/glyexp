#' Check if an Object is an Experiment
#'
#' This function checks if an object is an experiment,
#' i.e. if it inherits from the class `glyexp_experiment`.
#'
#' @param x An object to check.
#' @return A logical value.
#' @export
is_experiment <- function(x) {
  return(inherits(x, "glyexp_experiment"))
}
