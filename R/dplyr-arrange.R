#' Arrange Sample or Variable Information
#'
#' @description
#' Arrange the sample or variable information tibble of an [experiment()].
#'
#' The same syntax as `dplyr::arrange()` is used.
#' For example, to arrange samples by the "group" column,
#' use `arrange_obs(exp, group)`.
#' This actually calls `dplyr::arrange()` on the sample information tibble
#' with the `group` column,
#' and then updates the expression matrix accordingly to match the new order.
#'
#' @param exp An [experiment()].
#' @param ... <[`data-masking`][rlang::args_data_masking]> Variables to arrange by,
#'   passed to `dplyr::arrange()` internally.
#'
#' @return An new [experiment()] object.
#'
#' @examples
#' # Create a toy experiment for demonstration
#' exp <- toy_experiment
#' # Add a type column to the variable information for demonstration
#' exp$var_info$type <- c("Y", "X", "Z", "Y")
#'
#' # Arrange samples by group column
#' arranged_exp <- arrange_obs(exp, group)
#' arranged_exp$sample_info
#' arranged_exp$expr_mat
#'
#' # Arrange variables by type column  
#' arranged_exp <- arrange_var(exp, type)
#' arranged_exp$var_info
#' arranged_exp$expr_mat
#'
#' # Arrange by multiple columns
#' arrange_obs(exp, group, sample)$sample_info
#'
#' @export
arrange_obs <- function(exp, ...) {
  arrange_info_data(
    exp = exp,
    info_field = "sample_info",
    id_column = "sample",
    matrix_updater = function(mat, ids) mat[, ids, drop = FALSE],
    ...
  )
}


#' @rdname arrange_obs
#' @export
arrange_var <- function(exp, ...) {
  arrange_info_data(
    exp = exp,
    info_field = "var_info",
    id_column = "variable",
    matrix_updater = function(mat, ids) mat[ids, , drop = FALSE],
    ...
  )
}


# Internal function that handles the common logic for both arrange_obs and arrange_var
arrange_info_data <- function(exp, info_field, id_column, matrix_updater, ...) {
  stopifnot(is_experiment(exp))
  
  # Get original data and arrange it
  original_data <- exp[[info_field]]
  new_data <- try_arrange(original_data, info_field, ...)
  
  # Update the expression matrix using the new order
  new_ids <- new_data[[id_column]]
  new_expr_mat <- matrix_updater(exp$expr_mat, new_ids)
  
  # Create new experiment object
  new_exp <- exp
  new_exp[[info_field]] <- new_data
  new_exp$expr_mat <- new_expr_mat
  
  new_exp
}


# Helper function to find arrange function calls in the call stack
find_arrange_call <- function() {
  find_user_call(c("arrange_obs", "arrange_var"))
}


# Wrapper for dplyr::arrange() that provides better error messages
try_arrange <- function(data, data_type, ...) {
  tryCatch(
    dplyr::arrange(data, ...),
    error = function(e) {
      if (grepl("object '.*' not found", conditionMessage(e))) {
        missing_col <- stringr::str_extract(conditionMessage(e), "'(.*)' not found", group = 1)
        available_cols <- colnames(data)
        cli::cli_abort(c(
          "Column {.field {missing_col}} not found in `{data_type}`.",
          "i" = "Available columns: {.field {available_cols}}"
        ), call = find_arrange_call())
      } else {
        cli::cli_abort(conditionMessage(e), call = find_arrange_call())
      }
    }
  )
}
