#' Arrange Sample or Variable Information
#'
#' @description
#' Arrange the sample or variable information tibble of an [experiment()].
#'
#' The same syntax as `dplyr::arrange()` is used.
#' For example, to arrange samples by the "group" column,
#' use `arrange_samples(exp, group)`.
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
#' expr_mat <- matrix(1:25, nrow = 5)
#' colnames(expr_mat) <- paste0("S", 1:5)
#' rownames(expr_mat) <- paste0("V", 1:5)
#' sample_info <- tibble::tibble(
#'   sample = paste0("S", 1:5),
#'   group = c("B", "A", "C", "A", "B")
#' )
#' var_info <- tibble::tibble(
#'   variable = paste0("V", 1:5),
#'   type = c("Y", "X", "Z", "X", "Y")
#' )
#' exp <- experiment(expr_mat, sample_info, var_info)
#'
#' # Arrange samples by group column
#' arranged_exp <- arrange_samples(exp, group)
#' arranged_exp$sample_info
#' arranged_exp$expr_mat
#'
#' # Arrange variables by type column  
#' arranged_exp <- arrange_variables(exp, type)
#' arranged_exp$var_info
#' arranged_exp$expr_mat
#'
#' # Arrange by multiple columns
#' arrange_samples(exp, group, sample)$sample_info
#'
#' @export
arrange_samples <- function(exp, ...) {
  arrange_info_data(
    exp = exp,
    info_field = "sample_info",
    id_column = "sample",
    matrix_updater = function(mat, ids) mat[, ids, drop = FALSE],
    ...
  )
}


#' @rdname arrange_samples
#' @export
arrange_variables <- function(exp, ...) {
  arrange_info_data(
    exp = exp,
    info_field = "var_info",
    id_column = "variable",
    matrix_updater = function(mat, ids) mat[ids, , drop = FALSE],
    ...
  )
}


# Internal function that handles the common logic for both arrange_samples and arrange_variables
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
        ), call = find_user_call())
      } else {
        cli::cli_abort(conditionMessage(e), call = find_user_call())
      }
    }
  )
}
