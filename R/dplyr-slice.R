#' Slice sample or variable information
#'
#' @description
#' Slice the sample or variable information of an [experiment()] or
#' `SummarizedExperiment`.
#'
#' These functions provide row-wise slicing operations similar to dplyr's slice functions.
#' They select rows by position or based on values in specified columns,
#' and update the expression matrix accordingly to match the new selection.
#'
#' - `slice_col()` and `slice_row()`: Select rows by position
#' - `slice_head_col()` and `slice_head_row()`: Select first n rows
#' - `slice_tail_col()` and `slice_tail_row()`: Select last n rows
#' - `slice_sample_col()` and `slice_sample_row()`: Select random n rows
#' - `slice_max_col()` and `slice_max_row()`: Select rows with highest values
#' - `slice_min_col()` and `slice_min_row()`: Select rows with lowest values
#'
#' @param exp An [experiment()] or `SummarizedExperiment` object.
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
#' @return An object of the same class as `exp`.
#'
#' @inheritSection mutate_col Identifier columns
#' @examples
#' # Add values used for slicing to a bundled experiment
#' exp <- real_experiment |>
#'   mutate_col(score = seq_len(dplyr::n())) |>
#'   mutate_row(value = seq_len(dplyr::n()))
#'
#' # Select specific rows by position
#' slice_col(exp, 1, 3, 5)
#'
#' # Select first 3 samples
#' slice_head_col(exp, n = 3)
#'
#' # Select last 2 variables
#' slice_tail_row(exp, n = 2)
#'
#' # Select 2 random samples
#' slice_sample_col(exp, n = 2)
#'
#' # Select samples with highest scores
#' slice_max_col(exp, order_by = score, n = 2)
#'
#' # Select variables with lowest values
#' slice_min_row(exp, order_by = value, n = 2)
#'
#' @export
slice_col <- function(exp, ...) {
  slice_info_data(
    exp = exp,
    info_field = "sample_info",
    id_column = "sample",
    matrix_updater = function(mat, ids) mat[, ids, drop = FALSE],
    slice_fun = dplyr::slice,
    ...
  )
}

#' @rdname slice_col
#' @export
slice_row <- function(exp, ...) {
  slice_info_data(
    exp = exp,
    info_field = "var_info",
    id_column = "variable",
    matrix_updater = function(mat, ids) mat[ids, , drop = FALSE],
    slice_fun = dplyr::slice,
    ...
  )
}

#' @rdname slice_col
#' @export
slice_head_col <- function(exp, n, prop) {
  args <- list()
  if (!missing(n)) {
    args$n <- n
  }
  if (!missing(prop)) {
    args$prop <- prop
  }

  do.call(
    slice_info_data,
    c(
      list(
        exp = exp,
        info_field = "sample_info",
        id_column = "sample",
        matrix_updater = function(mat, ids) mat[, ids, drop = FALSE],
        slice_fun = dplyr::slice_head
      ),
      args
    )
  )
}

#' @rdname slice_col
#' @export
slice_head_row <- function(exp, n, prop) {
  args <- list()
  if (!missing(n)) {
    args$n <- n
  }
  if (!missing(prop)) {
    args$prop <- prop
  }

  do.call(
    slice_info_data,
    c(
      list(
        exp = exp,
        info_field = "var_info",
        id_column = "variable",
        matrix_updater = function(mat, ids) mat[ids, , drop = FALSE],
        slice_fun = dplyr::slice_head
      ),
      args
    )
  )
}

#' @rdname slice_col
#' @export
slice_tail_col <- function(exp, n, prop) {
  args <- list()
  if (!missing(n)) {
    args$n <- n
  }
  if (!missing(prop)) {
    args$prop <- prop
  }

  do.call(
    slice_info_data,
    c(
      list(
        exp = exp,
        info_field = "sample_info",
        id_column = "sample",
        matrix_updater = function(mat, ids) mat[, ids, drop = FALSE],
        slice_fun = dplyr::slice_tail
      ),
      args
    )
  )
}

#' @rdname slice_col
#' @export
slice_tail_row <- function(exp, n, prop) {
  args <- list()
  if (!missing(n)) {
    args$n <- n
  }
  if (!missing(prop)) {
    args$prop <- prop
  }

  do.call(
    slice_info_data,
    c(
      list(
        exp = exp,
        info_field = "var_info",
        id_column = "variable",
        matrix_updater = function(mat, ids) mat[ids, , drop = FALSE],
        slice_fun = dplyr::slice_tail
      ),
      args
    )
  )
}

#' @rdname slice_col
#' @export
slice_sample_col <- function(exp, n, prop, weight_by = NULL, replace = FALSE) {
  args <- list()
  if (!missing(n)) {
    args$n <- n
  }
  if (!missing(prop)) {
    args$prop <- prop
  }

  # Handle weight_by with rlang quasiquotation
  if (!missing(weight_by)) {
    args$weight_by <- rlang::enquo(weight_by)
  }

  args$replace <- replace

  do.call(
    slice_info_data,
    c(
      list(
        exp = exp,
        info_field = "sample_info",
        id_column = "sample",
        matrix_updater = function(mat, ids) mat[, ids, drop = FALSE],
        slice_fun = dplyr::slice_sample
      ),
      args
    )
  )
}

#' @rdname slice_col
#' @export
slice_sample_row <- function(exp, n, prop, weight_by = NULL, replace = FALSE) {
  args <- list()
  if (!missing(n)) {
    args$n <- n
  }
  if (!missing(prop)) {
    args$prop <- prop
  }

  # Handle weight_by with rlang quasiquotation
  if (!missing(weight_by)) {
    args$weight_by <- rlang::enquo(weight_by)
  }

  args$replace <- replace

  do.call(
    slice_info_data,
    c(
      list(
        exp = exp,
        info_field = "var_info",
        id_column = "variable",
        matrix_updater = function(mat, ids) mat[ids, , drop = FALSE],
        slice_fun = dplyr::slice_sample
      ),
      args
    )
  )
}

#' @rdname slice_col
#' @export
slice_max_col <- function(
  exp,
  order_by,
  ...,
  n,
  prop,
  with_ties = TRUE,
  na_rm = FALSE
) {
  args <- list(order_by = rlang::enquo(order_by))
  if (!missing(n)) {
    args$n <- n
  }
  if (!missing(prop)) {
    args$prop <- prop
  }
  args$with_ties <- with_ties
  args$na_rm <- na_rm
  dots <- list(...)
  args <- c(args, dots)

  do.call(
    slice_info_data,
    c(
      list(
        exp = exp,
        info_field = "sample_info",
        id_column = "sample",
        matrix_updater = function(mat, ids) mat[, ids, drop = FALSE],
        slice_fun = dplyr::slice_max
      ),
      args
    )
  )
}

#' @rdname slice_col
#' @export
slice_max_row <- function(
  exp,
  order_by,
  ...,
  n,
  prop,
  with_ties = TRUE,
  na_rm = FALSE
) {
  args <- list(order_by = rlang::enquo(order_by))
  if (!missing(n)) {
    args$n <- n
  }
  if (!missing(prop)) {
    args$prop <- prop
  }
  args$with_ties <- with_ties
  args$na_rm <- na_rm
  dots <- list(...)
  args <- c(args, dots)

  do.call(
    slice_info_data,
    c(
      list(
        exp = exp,
        info_field = "var_info",
        id_column = "variable",
        matrix_updater = function(mat, ids) mat[ids, , drop = FALSE],
        slice_fun = dplyr::slice_max
      ),
      args
    )
  )
}

#' @rdname slice_col
#' @export
slice_min_col <- function(
  exp,
  order_by,
  ...,
  n,
  prop,
  with_ties = TRUE,
  na_rm = FALSE
) {
  args <- list(order_by = rlang::enquo(order_by))
  if (!missing(n)) {
    args$n <- n
  }
  if (!missing(prop)) {
    args$prop <- prop
  }
  args$with_ties <- with_ties
  args$na_rm <- na_rm
  dots <- list(...)
  args <- c(args, dots)

  do.call(
    slice_info_data,
    c(
      list(
        exp = exp,
        info_field = "sample_info",
        id_column = "sample",
        matrix_updater = function(mat, ids) mat[, ids, drop = FALSE],
        slice_fun = dplyr::slice_min
      ),
      args
    )
  )
}

#' @rdname slice_col
#' @export
slice_min_row <- function(
  exp,
  order_by,
  ...,
  n,
  prop,
  with_ties = TRUE,
  na_rm = FALSE
) {
  args <- list(order_by = rlang::enquo(order_by))
  if (!missing(n)) {
    args$n <- n
  }
  if (!missing(prop)) {
    args$prop <- prop
  }
  args$with_ties <- with_ties
  args$na_rm <- na_rm
  dots <- list(...)
  args <- c(args, dots)

  do.call(
    slice_info_data,
    c(
      list(
        exp = exp,
        info_field = "var_info",
        id_column = "variable",
        matrix_updater = function(mat, ids) mat[ids, , drop = FALSE],
        slice_fun = dplyr::slice_min
      ),
      args
    )
  )
}

# Internal function that handles the common logic for slice operations
slice_info_data <- function(
  exp,
  info_field,
  id_column,
  matrix_updater,
  slice_fun,
  ...
) {
  stopifnot(is_tidy_container(exp))
  id_column <- tidy_id_column(exp, id_column)

  # Get original data and slice it
  original_data <- tidy_info_data(exp, info_field, id_column)
  new_data <- try_slice(original_data, info_field, slice_fun, ...)

  if (methods::is(exp, "SummarizedExperiment")) {
    return(update_se_info(exp, new_data, info_field, id_column, subset = TRUE))
  }

  # Update the expression matrix using the new order
  new_ids <- new_data[[id_column]]
  new_expr_mat <- matrix_updater(exp$expr_mat, new_ids)

  # Create new experiment object
  new_exp <- exp
  new_exp[[info_field]] <- new_data
  new_exp$expr_mat <- new_expr_mat

  new_exp
}

# Wrapper for dplyr slice functions that provides better error messages
try_slice <- function(data, data_type, slice_fun, ...) {
  tryCatch(
    slice_fun(data, ...),
    error = function(e) {
      error_msg <- conditionMessage(e)
      missing_col <- extract_missing_column(error_msg)
      if (!is.na(missing_col)) {
        abort_missing_column(missing_col, data_type, colnames(data))
      }
      cli::cli_abort(error_msg, call = NULL)
    }
  )
}
