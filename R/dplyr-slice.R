#' Slice Sample or Variable Information
#'
#' @description
#' Slice the sample or variable information tibble of an [experiment()].
#'
#' These functions provide row-wise slicing operations similar to dplyr's slice functions.
#' They select rows by position or based on values in specified columns,
#' and update the expression matrix accordingly to match the new selection.
#'
#' - `slice_samples()` and `slice_variables()`: Select rows by position
#' - `slice_head_samples()` and `slice_head_variables()`: Select first n rows
#' - `slice_tail_samples()` and `slice_tail_variables()`: Select last n rows
#' - `slice_sample_samples()` and `slice_sample_variables()`: Select random n rows
#' - `slice_max_samples()` and `slice_max_variables()`: Select rows with highest values
#' - `slice_min_samples()` and `slice_min_variables()`: Select rows with lowest values
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
#' expr_mat <- matrix(1:25, nrow = 5)
#' colnames(expr_mat) <- paste0("S", 1:5)
#' rownames(expr_mat) <- paste0("V", 1:5)
#' sample_info <- tibble::tibble(
#'   sample = paste0("S", 1:5),
#'   group = c("A", "B", "C", "A", "B"),
#'   score = c(10, 20, 30, 15, 25)
#' )
#' var_info <- tibble::tibble(
#'   variable = paste0("V", 1:5),
#'   type = c("X", "Y", "Z", "X", "Y"),
#'   value = c(5, 10, 15, 8, 12)
#' )
#' exp <- experiment(expr_mat, sample_info, var_info)
#'
#' # Select specific rows by position
#' slice_samples(exp, 1, 3, 5)
#'
#' # Select first 3 samples
#' slice_head_samples(exp, n = 3)
#'
#' # Select last 2 variables
#' slice_tail_variables(exp, n = 2)
#'
#' # Select 2 random samples
#' slice_sample_samples(exp, n = 2)
#'
#' # Select samples with highest scores
#' slice_max_samples(exp, order_by = score, n = 2)
#'
#' # Select variables with lowest values
#' slice_min_variables(exp, order_by = value, n = 2)
#'
#' @export
slice_samples <- function(exp, ...) {
  slice_info_data(
    exp = exp,
    info_field = "sample_info",
    id_column = "sample",
    matrix_updater = function(mat, ids) mat[, ids, drop = FALSE],
    slice_fun = dplyr::slice,
    ...
  )
}

#' @rdname slice_samples
#' @export
slice_variables <- function(exp, ...) {
  slice_info_data(
    exp = exp,
    info_field = "var_info",
    id_column = "variable",
    matrix_updater = function(mat, ids) mat[ids, , drop = FALSE],
    slice_fun = dplyr::slice,
    ...
  )
}

#' @rdname slice_samples
#' @export
slice_head_samples <- function(exp, n, prop) {
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

#' @rdname slice_samples
#' @export
slice_head_variables <- function(exp, n, prop) {
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

#' @rdname slice_samples
#' @export
slice_tail_samples <- function(exp, n, prop) {
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

#' @rdname slice_samples
#' @export
slice_tail_variables <- function(exp, n, prop) {
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

#' @rdname slice_samples
#' @export
slice_sample_samples <- function(exp, n, prop, weight_by = NULL, replace = FALSE) {
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

#' @rdname slice_samples
#' @export
slice_sample_variables <- function(exp, n, prop, weight_by = NULL, replace = FALSE) {
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

#' @rdname slice_samples
#' @export
slice_max_samples <- function(exp, order_by, ..., n, prop, with_ties = TRUE, na_rm = FALSE) {
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

#' @rdname slice_samples
#' @export
slice_max_variables <- function(exp, order_by, ..., n, prop, with_ties = TRUE, na_rm = FALSE) {
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

#' @rdname slice_samples
#' @export
slice_min_samples <- function(exp, order_by, ..., n, prop, with_ties = TRUE, na_rm = FALSE) {
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

#' @rdname slice_samples
#' @export
slice_min_variables <- function(exp, order_by, ..., n, prop, with_ties = TRUE, na_rm = FALSE) {
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
    "slice_samples", "slice_variables",
    "slice_head_samples", "slice_head_variables", 
    "slice_tail_samples", "slice_tail_variables",
    "slice_sample_samples", "slice_sample_variables",
    "slice_max_samples", "slice_max_variables",
    "slice_min_samples", "slice_min_variables"
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