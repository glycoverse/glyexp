#' Subsetting experiments
#'
#' @description
#' Getting a subset of an experiment object.
#' Subsetting is first done on the expression matrix,
#' then the sample information and variable information tibbles are filtered
#' and ordered accordingly.
#'
#' Syntax for `[` is similar to subsetting a matrix,
#' with some differences:
#' - Both row and column indices are required, i.e. `exp[i]` is not allowed,
#'   but `exp[i, ]` and `exp[, j]` are allowed.
#' - `drop` argument is not supported.
#'   Subsetting an experiment always returns an new experiment,
#'   even if it has only one sample or one variable.
#' - Renaming the subsetted experiment is no longer supported.
#'
#' Assigning to a subset of an experiment is not allowed,
#' i.e., `exp[1, 1[ <- 0` will raise an error.
#' You can create a new experiment with new data if needed.
#'
#' @param x An [experiment()].
#' @param i,j Row (variable) and column (sample) indices to subset.
#' @param ... Ignored.
#' @param value Ignored.
#'
#' @return An [experiment()] object.
#'
#' @examples
#' # Create a toy experiment for demonstration
#' exp <- toy_experiment
#'
#' # Subsetting single samples
#' exp[, "S1"]
#' exp[, 1]
#'
#' # Subsetting single variables
#' exp["V1", ]
#' exp[1, ]
#'
#' # Subsetting multiple samples and variables
#' exp[c("V1", "V2"), c("S2", "S3")]
#' exp[c(1, 2), c(2, 3)]
#'
#' # Create a copy
#' exp[, ]
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
`[.glyexp_experiment` <- function(x, i, j, ...) {
  stopifnot(is_experiment(x))
  # forbid `exp[i]`
  if (nargs() == 2 && missing(j)) {
    cli::cli_abort("Subsetting an experiment requires both row and column indices.")
  }
  # update expr_mat
  sub_expr_mat <- x$expr_mat[i, j, drop = FALSE]
  # update sample_info
  sub_sample_info <- x$sample_info %>%
    dplyr::filter(.data$sample %in% colnames(sub_expr_mat)) %>%
    dplyr::arrange(match(.data$sample, colnames(sub_expr_mat)))
  # update var_info
  sub_var_info <- x$var_info %>%
    dplyr::filter(.data$variable %in% rownames(sub_expr_mat)) %>%
    dplyr::arrange(match(.data$variable, rownames(sub_expr_mat)))

  # create new experiment
  new_exp <- x
  new_exp$expr_mat <- sub_expr_mat
  new_exp$sample_info <- sub_sample_info
  new_exp$var_info <- sub_var_info
  new_exp
}


#' @rdname sub-.glyexp_experiment
#' @export
`[<-.glyexp_experiment` <- function(x, i, j, ..., value) {
  stopifnot(is_experiment(x))
  cli::cli_abort("Subsetting an experiment is read-only.")
}
