#' Filter samples or variables of an experiment
#'
#' @description
#' Getting a subset of an [experiment()] by filtering samples or variables.
#'
#' The same syntax as [dplyr::filter()] is used.
#' For example, to get a subset of an experiment keeping only "HC" samples,
#' use `filter_obs(exp, group == "HC")`.
#' This actually calls `dplyr::filter()` on the sample information tibble
#' with condition `group == "HC"`,
#' and then updates the expression matrix accordingly.
#'
#' @param exp An [experiment()].
#' @param ... <[`data-masking`][rlang::args_data_masking]> Expression to filter samples or variables.
#'   passed to [dplyr::filter()] internally.
#' @param .drop_levels Logical. If `TRUE`, drop unused factor levels for columns
#'   referenced in the filtering expressions.
#'
#' @return An new [experiment()] object.
#'
#' @examples
#' # Create a toy experiment for demonstration
#' exp <- toy_experiment |>
#'   mutate_var(type = c("X", "X", "Y", "Y"))
#'
#' # Filter samples
#' sub_exp_1 <- filter_obs(exp, group == "A")
#' get_sample_info(sub_exp_1)
#' get_expr_mat(sub_exp_1)
#'
#' # Filter variables
#' sub_exp_2 <- filter_var(exp, type == "X")
#' get_var_info(sub_exp_2)
#' get_expr_mat(sub_exp_2)
#'
#' # Use pipe
#' sub_exp_3 <- exp |>
#'   filter_obs(group == "A") |>
#'   filter_var(type == "X")
#' get_sample_info(sub_exp_3)
#' get_var_info(sub_exp_3)
#' get_expr_mat(sub_exp_3)
#'
#' @export
filter_obs <- function(exp, ..., .drop_levels = FALSE) {
  filter_info_data(
    exp = exp,
    info_field = "sample_info",
    id_column = "sample",
    dim_name = "samples",
    matrix_updater = function(mat, ids) mat[, ids, drop = FALSE],
    ...,
    .drop_levels = .drop_levels
  )
}

#' @rdname filter_obs
#' @export
filter_var <- function(exp, ..., .drop_levels = FALSE) {
  filter_info_data(
    exp = exp,
    info_field = "var_info",
    id_column = "variable",
    dim_name = "variables",
    matrix_updater = function(mat, ids) mat[ids, , drop = FALSE],
    ...,
    .drop_levels = .drop_levels
  )
}

# Internal function that handles the common logic for both filter_obs and filter_var
filter_info_data <- function(exp, info_field, id_column, dim_name, matrix_updater, ..., .drop_levels = FALSE) {
  stopifnot(is_experiment(exp))

  # Get original data and filter it
  original_data <- exp[[info_field]]
  quos <- rlang::enquos(...)
  new_data <- try_filter(original_data, info_field, dim_name, quos)
  if (isTRUE(.drop_levels)) {
    new_data <- drop_filter_levels(new_data, original_data, quos)
  }

  # Update the expression matrix using the provided updater function
  new_ids <- new_data[[id_column]]
  new_expr_mat <- matrix_updater(exp$expr_mat, new_ids)

  # Create new experiment object
  new_exp <- exp
  new_exp[[info_field]] <- new_data
  new_exp$expr_mat <- new_expr_mat

  new_exp
}

try_filter <- function(data, data_type, dim_name, quos) {
  # data: `sample_info` or `var_info`
  # data_type: "sample_info" or "var_info", used in error messages
  # dim_name: "samples" or "variables", used in error messages
  # ...: arguments to pass to `dplyr::filter()`
  tryCatch(
    new_data <- dplyr::filter(data, !!!quos),
    error = function(e) {
      missing_col <- extract_missing_column(conditionMessage(e))
      if (!is.na(missing_col)) {
        abort_missing_column(missing_col, data_type, colnames(data))
      }
      stop(e)
    }
  )

  new_data
}

drop_filter_levels <- function(new_data, original_data, quos) {
  used_cols <- filter_expr_cols(original_data, quos)
  if (length(used_cols) == 0) {
    return(new_data)
  }

  used_cols <- used_cols[used_cols %in% colnames(new_data)]
  factor_cols <- used_cols[purrr::map_lgl(new_data[used_cols], is.factor)]
  if (length(factor_cols) == 0) {
    return(new_data)
  }

  new_data[factor_cols] <- purrr::map(new_data[factor_cols], droplevels)
  new_data
}

filter_expr_cols <- function(data, quos) {
  data_cols <- colnames(data)
  expr_cols <- purrr::map(quos, ~ expr_names(rlang::get_expr(.x)))
  expr_cols <- unique(unlist(expr_cols, use.names = FALSE))
  expr_cols <- intersect(expr_cols, data_cols)

  selected_cols <- purrr::map(
    quos,
    ~ if_any_cols_from_expr(rlang::get_expr(.x), data, rlang::get_env(.x))
  )
  selected_cols <- unique(unlist(selected_cols, use.names = FALSE))

  unique(c(expr_cols, selected_cols))
}

expr_names <- function(expr) {
  if (rlang::is_symbol(expr)) {
    return(as.character(expr))
  }
  if (!rlang::is_call(expr)) {
    return(character())
  }

  args <- rlang::call_args(expr)
  names <- purrr::map(args, expr_names)
  unique(unlist(names, use.names = FALSE))
}

if_any_cols_from_expr <- function(expr, data, env) {
  calls <- find_if_any_calls(expr)
  if (length(calls) == 0) {
    return(character())
  }

  selected_cols <- purrr::map(calls, ~ extract_if_any_cols(.x, data, env))
  unique(unlist(selected_cols, use.names = FALSE))
}

find_if_any_calls <- function(expr) {
  if (!rlang::is_call(expr)) {
    return(list())
  }

  call_name <- rlang::call_name(expr)
  args <- rlang::call_args(expr)
  nested_calls <- purrr::flatten(purrr::map(args, find_if_any_calls))

  if (call_name %in% c("if_any", "if_all")) {
    c(list(expr), nested_calls)
  } else {
    nested_calls
  }
}

extract_if_any_cols <- function(expr, data, env) {
  args <- rlang::call_args(expr)
  cols_expr <- args$.cols
  if (is.null(cols_expr) && length(args) > 0) {
    cols_expr <- args[[1]]
  }
  if (is.null(cols_expr)) {
    return(character())
  }

  selection <- tidyselect::eval_select(rlang::new_quosure(cols_expr, env = env), data)
  names(selection)
}
