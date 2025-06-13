#' Mutate Sample or Variable Information
#'
#' @description
#' Mutate the sample or variable information tibble of an [experiment()].
#'
#' The same syntax as `dplyr::mutate()` is used.
#' For example, to add a new column to the sample information tibble,
#' use `mutate_samples(exp, new_column = value)`.
#' This actually calls `dplyr::mutate()` on the sample information tibble
#' with `new_column = value`.
#'
#' If the `sample` column in `sample_info` or the `variable` column in `var_info`
#' is to be modified, the new column must be unique,
#' otherwise an error is thrown.
#' The column names or row names of `expr_mat` will be updated accordingly.
#'
#' @param exp An [experiment()].
#' @param ... <[`data-masking`][rlang::args_data_masking]> Name-value pairs,
#'   passed to `dplyr::mutate()` internally.
#'
#' @return An new [experiment()] object.
#'
#' @examples
#' # Create a toy experiment for demonstration
#' expr_mat <- matrix(1:25, nrow = 5)
#' colnames(expr_mat) <- paste0("S", 1:5)
#' rownames(expr_mat) <- paste0("V", 1:5)
#' sample_info <- tibble::tibble(
#'   sample = paste0("S", 1:5),
#'   group = c("A", "A", "A", "B", "B")
#' )
#' var_info <- tibble::tibble(
#'   variable = paste0("V", 1:5),
#'   type = c("X", "X", "Y", "Y", "Y")
#' )
#' exp <- experiment(expr_mat, sample_info, var_info)
#'
#' # Add a new column to sample information tibble or variable information tibble
#' mutate_samples(exp, new_column = c(1, 2, 3, 4, 5))$sample_info
#' mutate_variables(exp, new_column = c("A", "A", "B", "B", "B"))$var_info
#'
#' # Modify existing columns
#' mutate_samples(exp, group = dplyr::if_else(group == "A", "good", "bad"))$sample_info
#' mutate_variables(exp, type = dplyr::if_else(type == "X", "good", "bad"))$var_info
#'
#' # Modify the `sample` column in sample information tibble
#' new_exp <- mutate_samples(exp, sample = c("SI", "SII", "SIII", "SIV", "SV"))
#' new_exp$sample_info
#' new_exp$expr_mat
#'
#' # Modify the `variable` column in variable information tibble
#' new_exp <- mutate_variables(exp, variable = c("VI", "VII", "VIII", "VIV", "VV"))
#' new_exp$var_info
#' new_exp$expr_mat
#'
#' @export
mutate_samples <- function(exp, ...) {
  stopifnot(is_experiment(exp))
  new_sample_info <- mutate_data(exp$sample_info, "sample_info", call = rlang::expr(mutate_samples()), ...)
  if (!identical(new_sample_info$sample, exp$sample_info$sample)) {
    if (dplyr::n_distinct(new_sample_info$sample) != nrow(exp$sample_info)) {
      cli::cli_abort("Column sample in `sample_info` must be unique.")
    }
    new_expr_mat <- exp$expr_mat
    colnames(new_expr_mat) <- new_sample_info$sample
  } else {
    new_expr_mat <- exp$expr_mat
  }

  new_exp <- exp
  new_exp$sample_info <- new_sample_info
  new_exp$expr_mat <- new_expr_mat

  new_exp
}


#' @rdname mutate_samples
#' @export
mutate_variables <- function(exp, ...) {
  stopifnot(is_experiment(exp))
  new_var_info <- mutate_data(exp$var_info, "var_info", call = rlang::expr(mutate_variables()), ...)
  if (!identical(new_var_info$variable, exp$var_info$variable)) {
    if (dplyr::n_distinct(new_var_info$variable) != nrow(exp$var_info)) {
      cli::cli_abort("Column variable in `var_info` must be unique.")
    }
    new_expr_mat <- exp$expr_mat
    rownames(new_expr_mat) <- new_var_info$variable
  } else {
    new_expr_mat <- exp$expr_mat
  }

  new_exp <- exp
  new_exp$var_info <- new_var_info
  new_exp$expr_mat <- new_expr_mat

  new_exp
}


mutate_data <- function(data, data_type, call, ...) {
  tryCatch(
    dplyr::mutate(data, ...),
    error = function(e) {
      if (grepl("object '.*' not found", conditionMessage(e))) {
        missing_col <- stringr::str_extract(conditionMessage(e), "'(.*)' not found", group = 1)
        available_cols <- colnames(data)
        cli::cli_abort(c(
          "Column {.field {missing_col}} not found in `{data_type}`.",
          "i" = "Available columns: {.field {available_cols}}"
        ), call = call)
      } else {
        stop(e)
      }
    }
  )
}
