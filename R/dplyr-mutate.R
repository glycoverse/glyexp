#' Mutate sample or variable information
#'
#' @description
#' Mutate the sample or variable information of an [experiment()] or
#' `SummarizedExperiment`.
#'
#' The same syntax as `dplyr::mutate()` is used.
#' For example, to add a new column to the sample information tibble,
#' use `mutate_obs(exp, new_column = value)`.
#' This actually calls `dplyr::mutate()` on the sample information tibble
#' with `new_column = value`.
#'
#' If the `sample` column in `sample_info` or the `variable` column in `var_info`
#' is to be modified, the new column must be unique,
#' otherwise an error is thrown.
#' The assay column names or row names will be updated accordingly.
#'
#' @param exp An [experiment()] or `SummarizedExperiment` object.
#' @param ... <[`data-masking`][rlang::args_data_masking]> Name-value pairs,
#'   passed to `dplyr::mutate()` internally.
#'
#' @return An object of the same class as `exp`.
#'
#' @examples
#' # Create a toy experiment for demonstration
#' exp <- toy_experiment |>
#'   mutate_var(type = c("X", "X", "Y", "Y"))
#'
#' # Add a new column to sample information tibble or variable information tibble
#' exp |>
#'   mutate_obs(new_column = c(1, 2, 3, 4, 5, 6)) |>
#'   get_sample_info()
#'
#' exp |>
#'   mutate_var(new_column = c("A", "A", "B", "B")) |>
#'   get_var_info()
#'
#' # Modify existing columns
#' exp |>
#'   mutate_obs(group = dplyr::if_else(group == "A", "good", "bad")) |>
#'   get_sample_info()
#'
#' exp |>
#'   mutate_var(type = dplyr::if_else(type == "X", "good", "bad")) |>
#'   get_var_info()
#'
#' # Modify the `sample` column in sample information tibble
#' new_exp <- mutate_obs(exp, sample = c("SI", "SII", "SIII", "SIV", "SV", "SVI"))
#' get_sample_info(new_exp)
#' get_expr_mat(new_exp)
#'
#' # Modify the `variable` column in variable information tibble
#' new_exp <- mutate_var(exp, variable = c("VI", "VII", "VIII", "VIV"))
#' get_var_info(new_exp)
#' get_expr_mat(new_exp)
#'
#' @export
mutate_obs <- function(exp, ...) {
  mutate_info_data(
    exp = exp,
    info_type = "sample",
    info_field = "sample_info",
    id_column = "sample",
    matrix_dimnames_setter = function(mat, new_names) {
      colnames(mat) <- new_names
      mat
    },
    ...
  )
}


#' @rdname mutate_obs
#' @export
mutate_var <- function(exp, ...) {
  mutate_info_data(
    exp = exp,
    info_type = "variable",
    info_field = "var_info",
    id_column = "variable",
    matrix_dimnames_setter = function(mat, new_names) {
      rownames(mat) <- new_names
      mat
    },
    ...
  )
}


# Internal function that handles the common logic for both mutate_obs and mutate_var
mutate_info_data <- function(
  exp,
  info_type,
  info_field,
  id_column,
  matrix_dimnames_setter,
  ...
) {
  stopifnot(is_tidy_container(exp))

  # Get original data and mutate it
  original_data <- tidy_info_data(exp, info_field, id_column)
  new_data <- try_mutate(original_data, info_field, ...)

  # Check if the ID column was modified
  original_ids <- original_data[[id_column]]
  new_ids <- new_data[[id_column]]

  if (!identical(new_ids, original_ids)) {
    # Validate uniqueness of new IDs
    if (dplyr::n_distinct(new_ids) != nrow(original_data)) {
      cli::cli_abort(
        "Column {id_column} in `{info_field}` must be unique.",
        call = NULL
      )
    }
    # Update matrix dimnames
    if (is_experiment(exp)) {
      new_expr_mat <- matrix_dimnames_setter(exp$expr_mat, new_ids)
    }
  } else {
    if (is_experiment(exp)) {
      new_expr_mat <- exp$expr_mat
    }
  }

  if (methods::is(exp, "SummarizedExperiment")) {
    return(update_se_info(exp, new_data, info_field, id_column))
  }

  # Create new experiment object
  new_exp <- exp
  new_exp[[info_field]] <- new_data
  new_exp$expr_mat <- new_expr_mat

  new_exp
}

# Wrapper for dplyr::mutate() that provides better error messages
try_mutate <- function(data, data_type, ...) {
  tryCatch(
    dplyr::mutate(data, ...),
    error = function(e) {
      missing_col <- extract_missing_column(conditionMessage(e))
      if (!is.na(missing_col)) {
        abort_missing_column(missing_col, data_type, colnames(data))
      }
      stop(e)
    }
  )
}
