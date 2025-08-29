#' Slice Sample or Variable Information
#'
#' @description
#' Slice the sample or variable information tibble of an [experiment()].
#'
#' These functions provide row-wise slicing operations similar to dplyr's slice functions.
#' They select rows by position or based on values in specified columns,
#' and update the expression matrix accordingly to match the new selection.
#'
#' - `slice_obs()` and `slice_var()`: Select rows by position
#' - `slice_head_obs()` and `slice_head_var()`: Select first n rows
#' - `slice_tail_obs()` and `slice_tail_var()`: Select last n rows
#' - `slice_sample_obs()` and `slice_sample_var()`: Select random n rows
#' - `slice_max_obs()` and `slice_max_var()`: Select rows with highest values
#' - `slice_min_obs()` and `slice_min_var()`: Select rows with lowest values
#'
#' @param exp An [experiment()].
#' @param ... <[`data-masking`][rlang::args_data_masking]> For `slice_*()`,
#'   integer row positions. For `slice_max()` and `slice_min()`, variables to 
#'   order by. Other arguments passed to the corresponding dplyr function.
#' @param n For `slice_head()`, `slice_tail()`, `slice_sample()`, `slice_max()`, 
#'   and `slice_min()`, the number of rows to select.
#' @param prop For `slice_head()`, `slice_tail()`, `slice_sample()`, `slice_max()`, 
#'   and `slice_min()`, the proportion of rows to select.
#' @param order_by For `slice_max()` and `slice_min()`, variable to order by.
#' @param with_ties For `slice_max()` and `slice_min()`, should ties be kept?
#' @param na_rm For `slice_max()` and `slice_min()`, should missing values be removed?
#' @param weight_by For `slice_sample()`, sampling weights.
#' @param replace For `slice_sample()`, should sampling be with replacement?
#'
#' @return A new [experiment()] object.
#'
#' @examples
#' # Create a toy experiment for demonstration  
#' exp <- toy_experiment
#' # Add columns needed for demonstration
#' exp$sample_info$score <- c(10, 20, 30, 15, 25, 35)
#' exp$var_info$value <- c(5, 10, 15, 8)
#'
#' # Select specific rows by position
#' slice_obs(exp, 1, 3, 5)
#'
#' # Select first 3 samples
#' slice_head_obs(exp, n = 3)
#'
#' # Select last 2 variables
#' slice_tail_var(exp, n = 2)
#'
#' # Select 2 random samples
#' slice_sample_obs(exp, n = 2)
#'
#' # Select samples with highest scores
#' slice_max_obs(exp, order_by = score, n = 2)
#'
#' # Select variables with lowest values
#' slice_min_var(exp, order_by = value, n = 2)
#'
#' @export
slice_obs <- function(exp, ...) {
  slice_info_data(
    exp = exp,
    info_field = "sample_info",
    id_column = "sample",
    matrix_updater = function(mat, ids) mat[, ids, drop = FALSE],
    slice_fun = dplyr::slice,
    ...
  )
}

#' @rdname slice_obs
#' @export
slice_var <- function(exp, ...) {
  slice_info_data(
    exp = exp,
    info_field = "var_info",
    id_column = "variable",
    matrix_updater = function(mat, ids) mat[ids, , drop = FALSE],
    slice_fun = dplyr::slice,
    ...
  )
}

#' @rdname slice_obs
#' @export
slice_head_obs <- function(exp, n, prop) {
  args <- list()
  if (!missing(n)) args$n <- n
  if (!missing(prop)) args$prop <- prop
  
  do.call(slice_info_data, c(
    list(
      exp = exp,
      info_field = "sample_info",
      id_column = "sample",
      matrix_updater = function(mat, ids) mat[, ids, drop = FALSE],
      slice_fun = dplyr::slice_head
    ),
    args
  ))
}

#' @rdname slice_obs
#' @export
slice_head_var <- function(exp, n, prop) {
  args <- list()
  if (!missing(n)) args$n <- n
  if (!missing(prop)) args$prop <- prop
  
  do.call(slice_info_data, c(
    list(
      exp = exp,
      info_field = "var_info",
      id_column = "variable",
      matrix_updater = function(mat, ids) mat[ids, , drop = FALSE],
      slice_fun = dplyr::slice_head
    ),
    args
  ))
}

#' @rdname slice_obs
#' @export
slice_tail_obs <- function(exp, n, prop) {
  args <- list()
  if (!missing(n)) args$n <- n
  if (!missing(prop)) args$prop <- prop
  
  do.call(slice_info_data, c(
    list(
      exp = exp,
      info_field = "sample_info",
      id_column = "sample",
      matrix_updater = function(mat, ids) mat[, ids, drop = FALSE],
      slice_fun = dplyr::slice_tail
    ),
    args
  ))
}

#' @rdname slice_obs
#' @export
slice_tail_var <- function(exp, n, prop) {
  args <- list()
  if (!missing(n)) args$n <- n
  if (!missing(prop)) args$prop <- prop
  
  do.call(slice_info_data, c(
    list(
      exp = exp,
      info_field = "var_info",
      id_column = "variable",
      matrix_updater = function(mat, ids) mat[ids, , drop = FALSE],
      slice_fun = dplyr::slice_tail
    ),
    args
  ))
}

#' @rdname slice_obs
#' @export
slice_sample_obs <- function(exp, n, prop, weight_by = NULL, replace = FALSE) {
  args <- list()
  if (!missing(n)) args$n <- n
  if (!missing(prop)) args$prop <- prop
  
  # Handle weight_by with rlang quasiquotation
  if (!missing(weight_by)) {
    args$weight_by <- rlang::enquo(weight_by)
  }
  
  args$replace <- replace
  
  do.call(slice_info_data, c(
    list(
      exp = exp,
      info_field = "sample_info",
      id_column = "sample",
      matrix_updater = function(mat, ids) mat[, ids, drop = FALSE],
      slice_fun = dplyr::slice_sample
    ),
    args
  ))
}

#' @rdname slice_obs
#' @export
slice_sample_var <- function(exp, n, prop, weight_by = NULL, replace = FALSE) {
  args <- list()
  if (!missing(n)) args$n <- n
  if (!missing(prop)) args$prop <- prop
  
  # Handle weight_by with rlang quasiquotation
  if (!missing(weight_by)) {
    args$weight_by <- rlang::enquo(weight_by)
  }
  
  args$replace <- replace
  
  do.call(slice_info_data, c(
    list(
      exp = exp,
      info_field = "var_info",
      id_column = "variable",
      matrix_updater = function(mat, ids) mat[ids, , drop = FALSE],
      slice_fun = dplyr::slice_sample
    ),
    args
  ))
}

#' @rdname slice_obs
#' @export
slice_max_obs <- function(exp, order_by, ..., n, prop, with_ties = TRUE, na_rm = FALSE) {
  args <- list(order_by = rlang::enquo(order_by))
  if (!missing(n)) args$n <- n
  if (!missing(prop)) args$prop <- prop
  args$with_ties <- with_ties
  args$na_rm <- na_rm
  dots <- list(...)
  args <- c(args, dots)
  
  do.call(slice_info_data, c(
    list(
      exp = exp,
      info_field = "sample_info",
      id_column = "sample",
      matrix_updater = function(mat, ids) mat[, ids, drop = FALSE],
      slice_fun = dplyr::slice_max
    ),
    args
  ))
}

#' @rdname slice_obs
#' @export
slice_max_var <- function(exp, order_by, ..., n, prop, with_ties = TRUE, na_rm = FALSE) {
  args <- list(order_by = rlang::enquo(order_by))
  if (!missing(n)) args$n <- n
  if (!missing(prop)) args$prop <- prop
  args$with_ties <- with_ties
  args$na_rm <- na_rm
  dots <- list(...)
  args <- c(args, dots)
  
  do.call(slice_info_data, c(
    list(
      exp = exp,
      info_field = "var_info",
      id_column = "variable",
      matrix_updater = function(mat, ids) mat[ids, , drop = FALSE],
      slice_fun = dplyr::slice_max
    ),
    args
  ))
}

#' @rdname slice_obs
#' @export
slice_min_obs <- function(exp, order_by, ..., n, prop, with_ties = TRUE, na_rm = FALSE) {
  args <- list(order_by = rlang::enquo(order_by))
  if (!missing(n)) args$n <- n
  if (!missing(prop)) args$prop <- prop
  args$with_ties <- with_ties
  args$na_rm <- na_rm
  dots <- list(...)
  args <- c(args, dots)
  
  do.call(slice_info_data, c(
    list(
      exp = exp,
      info_field = "sample_info",
      id_column = "sample",
      matrix_updater = function(mat, ids) mat[, ids, drop = FALSE],
      slice_fun = dplyr::slice_min
    ),
    args
  ))
}

#' @rdname slice_obs
#' @export
slice_min_var <- function(exp, order_by, ..., n, prop, with_ties = TRUE, na_rm = FALSE) {
  args <- list(order_by = rlang::enquo(order_by))
  if (!missing(n)) args$n <- n
  if (!missing(prop)) args$prop <- prop
  args$with_ties <- with_ties
  args$na_rm <- na_rm
  dots <- list(...)
  args <- c(args, dots)
  
  do.call(slice_info_data, c(
    list(
      exp = exp,
      info_field = "var_info",
      id_column = "variable",
      matrix_updater = function(mat, ids) mat[ids, , drop = FALSE],
      slice_fun = dplyr::slice_min
    ),
    args
  ))
}

# Internal function that handles the common logic for slice operations
slice_info_data <- function(exp, info_field, id_column, matrix_updater, slice_fun, ...) {
  stopifnot(is_experiment(exp))
  
  # Get original data and slice it
  original_data <- exp[[info_field]]
  new_data <- try_slice(original_data, info_field, slice_fun, ...)
  
  # Update the expression matrix using the new order
  new_ids <- new_data[[id_column]]
  new_expr_mat <- matrix_updater(exp$expr_mat, new_ids)
  
  # Create new experiment object
  new_exp <- exp
  new_exp[[info_field]] <- new_data
  new_exp$expr_mat <- new_expr_mat
  
  new_exp
}

# Helper function to find slice function calls in the call stack
find_slice_call <- function() {
  find_user_call(c(
    "slice_obs", "slice_var",
    "slice_head_obs", "slice_head_var", 
    "slice_tail_obs", "slice_tail_var",
    "slice_sample_obs", "slice_sample_var",
    "slice_max_obs", "slice_max_var",
    "slice_min_obs", "slice_min_var"
  ))
}

# Wrapper for dplyr slice functions that provides better error messages
try_slice <- function(data, data_type, slice_fun, ...) {
  tryCatch(
    slice_fun(data, ...),
    error = function(e) {
      error_msg <- conditionMessage(e)
      
      # Handle various error patterns for missing columns  
      if (grepl("object '.*' not found", error_msg)) {
        # Extract column name - handle both single and multi-line error messages
        missing_col <- stringr::str_extract(error_msg, "object '([^']+)' not found")
        missing_col <- stringr::str_replace(missing_col, "object '([^']+)' not found", "\\1")
        if (is.na(missing_col)) {
          missing_col <- "unknown"
        }
        available_cols <- colnames(data)
        cli::cli_abort(c(
          "Column {.field {missing_col}} not found in `{data_type}`.",
          "i" = "Available columns: {.field {available_cols}}"
        ), call = find_slice_call())
      } else if (grepl("Column `.*` doesn't exist", error_msg)) {
        missing_col <- stringr::str_extract(error_msg, "Column `([^`]+)` doesn't exist")
        missing_col <- stringr::str_replace(missing_col, "Column `([^`]+)` doesn't exist", "\\1")
        if (is.na(missing_col)) {
          missing_col <- "unknown"
        }
        available_cols <- colnames(data)
        cli::cli_abort(c(
          "Column {.field {missing_col}} not found in `{data_type}`.",
          "i" = "Available columns: {.field {available_cols}}"
        ), call = find_slice_call())
      } else {
        cli::cli_abort(error_msg, call = find_slice_call())
      }
    }
  )
} 