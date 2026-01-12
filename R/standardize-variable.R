#' Standardize variable IDs in an experiment
#'
#' @description
#' Converts meaningless variable IDs (like "GP1", "V1") into meaningful,
#' human-readable IDs based on the experiment type and available columns.
#'
#' @param exp An [experiment()].
#' @param format A format string specifying how to construct variable IDs.
#'   Use `{column_name}` to insert values from `var_info` columns.
#'   If `NULL` (default), a sensible format is chosen based on `exp_type`.
#' @param unique_suffix A string pattern for making IDs unique when duplicates exist.
#'   Must contain `{N}` which will be replaced with the numeric suffix.
#'   Default is `"-{N}"`.
#'
#' @return The experiment with standardized variable IDs, invisibly.
#'
#' @examples
#' # See examples in the package documentation
#'
#' @export
standardize_variable <- function(exp, format = NULL, unique_suffix = "-{N}") {
  cli::cli_abort("{.fn standardize_variable} is not yet implemented.")
}
