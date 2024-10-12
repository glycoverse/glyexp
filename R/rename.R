#' Rename an Experiment
#'
#' Getting a new experiment with a new name.
#'
#' @param exp An experiment object.
#' @param new_name A character string with the new name.
#'
#' @return An experiment object with the new name.
#' @export
rename <- function(exp, new_name) {
  stopifnot(is_experiment(exp))
  stopifnot(is.character(new_name))
  exp$name <- new_name
  exp
}
