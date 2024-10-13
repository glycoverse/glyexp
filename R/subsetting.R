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
#' - A `name` argument is added to change the name of the subsetted experiment.
#'   If omitted, the name of the original experiment will be used.
#'
#' `[[` is not allowed for experiments currently.
#' Using it will raise an error.
#' You should always use `[` instead.
#'
#' Assigning to a subset of an experiment is not allowed,
#' i.e., `exp[1, 1[ <- 0` will raise an error.
#' You can create a new experiment with new data if needed.
#'
#' @param x An [experiment()].
#' @param i,j Row (variable) and column (sample) indices to subset.
#' @param name Name of the subsetted experiment.
#' @param ... Ignored.
#' @param value Ignored.
#'
#' @return An [experiment()] object.
#'
#' @examples
#' # Create a toy experiment for demonstration
#' expr_mat <- matrix(1:9, nrow = 3)
#' colnames(expr_mat) <- c("S1", "S2", "S3")
#' rownames(expr_mat) <- c("V1", "V2", "V3")
#' sample_info <- tibble::tibble(
#'   sample = c("S1", "S2", "S3"),
#'   group = rep("A", 3)
#' )
#' var_info <- tibble::tibble(
#'   variable = c("V1", "V2", "V3"),
#'   type = rep("B", 3)
#' )
#' exp <- experiment("my_exp", expr_mat, sample_info, var_info)
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
#' # Subsetting with a new name
#' exp[c("V1", "V2"), c("S2", "S3"), name = "sub_exp"]
#'
#' # Create a copy
#' exp[, ]
#'
#' @export
`[.glyexp_experiment` <- function(x, i, j, name = NULL, ...) {
  stopifnot(is_experiment(x))
  # forbid `exp[i]`
  if (nargs() == 2 && missing(j)) {
    cli::cli_abort("Subsetting an experiment requires both row and column indices.")
  }
  # update expr_mat
  sub_expr_mat <- x$expr_mat[i, j, drop = FALSE]
  # update sample_info
  sub_sample_info <- dplyr::filter(x$sample_info, sample %in% colnames(sub_expr_mat))
  sub_sample_info <- dplyr::arrange(sub_sample_info, match(sample, colnames(sub_expr_mat)))
  # update var_info
  sub_var_info <- dplyr::filter(x$var_info, variable %in% rownames(sub_expr_mat))
  sub_var_info <- dplyr::arrange(sub_var_info, match(variable, rownames(sub_expr_mat)))
  # update name
  if (is.null(name)) {
    new_name <- x$name
  } else {
    new_name <- name
  }
  # create new experiment
  new_experiment(new_name, sub_expr_mat, sub_sample_info, sub_var_info)
}


#' @rdname sub-.glyexp_experiment
#' @export
`[[.glyexp_experiment` <- function(x, i, j, ...) {
  stopifnot(is_experiment(x))
  cli::cli_abort(c(
    "Using `[[` with an experiment object is not allowed.",
    "i" = "Please use `[` instead."
  ))
}


#' @rdname sub-.glyexp_experiment
#' @export
`[<-.glyexp_experiment` <- function(x, i, j, ..., value) {
  stopifnot(is_experiment(x))
  cli::cli_abort("Subsetting an experiment is read-only.")
}


#' @rdname sub-.glyexp_experiment
#' @export
`[[<-.glyexp_experiment` <- function(x, i, j, ..., value) {
  stopifnot(is_experiment(x))
  cli::cli_abort("Subsetting an experiment is read-only.")
}


# Dismiss the "no visible binding" warning of R CMD check
utils::globalVariables(c("sample", "variable"))
