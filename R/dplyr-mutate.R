#' Mutate sample or variable information
#'
#' @description
#' Mutate the sample or variable information of an [experiment()] or
#' `SummarizedExperiment`.
#'
#' The same syntax as `dplyr::mutate()` is used.
#' For example, to add a new column to the sample information tibble,
#' use `mutate_col(exp, new_column = value)`.
#' This actually calls `dplyr::mutate()` on the sample information tibble
#' with `new_column = value`.
#'
#' If an identifier column is modified, its new values must be unique;
#' otherwise, an error is thrown.
#' The assay column names or row names will be updated accordingly.
#'
#' @param exp An [experiment()] or `SummarizedExperiment` object.
#' @param ... <[`data-masking`][rlang::args_data_masking]> Name-value pairs,
#'   passed to `dplyr::mutate()` internally.
#'
#' @return An object of the same class as `exp`.
#'
#' @section Identifier columns:
#' For an [experiment()] object, `sample` is a physical column in
#' `sample_info`, and `variable` is a physical column in `var_info`.
#'
#' For a `SummarizedExperiment`, sample and variable identifiers live in
#' `colnames(exp)` and `rownames(exp)`, rather than in
#' [SummarizedExperiment::colData()] or [SummarizedExperiment::rowData()].
#' Observation verbs expose `colnames(exp)` as a virtual `.sample` column, and
#' variable verbs expose `rownames(exp)` as a virtual `.variable` column. These
#' dot-prefixed names distinguish dimension identifiers from regular metadata
#' columns. After the operation, the virtual column is removed and its values
#' are written back to the corresponding dimension names.
#'
#' Consequently, `sample` in `colData(exp)` and `variable` in `rowData(exp)`
#' remain ordinary metadata columns. The names `.sample` and `.variable` are
#' reserved; an input containing either name in the corresponding metadata
#' raises an error rather than overwriting that column.
#'
#' If the corresponding dimension names are `NULL`, the virtual identifier is
#' unavailable and referring to it raises an error. `mutate_col(.sample = ...)`
#' or `mutate_row(.variable = ...)` can be used to create the missing names.
#'
#' @examples
#' library(SummarizedExperiment)
#'
#' # Add metadata to a bundled experiment
#' exp <- real_experiment |>
#'   mutate_row(type = "glycopeptide")
#'
#' # Add a new column to sample information tibble or variable information tibble
#' exp |>
#'   mutate_col(new_column = 1) |>
#'   colData()
#'
#' exp |>
#'   mutate_row(new_column = "A") |>
#'   rowData()
#'
#' # Modify existing columns
#' exp |>
#'   mutate_col(group = dplyr::if_else(group == "H", "healthy", "other")) |>
#'   colData()
#'
#' exp |>
#'   mutate_row(type = dplyr::if_else(type == "glycopeptide", "good", "bad")) |>
#'   rowData()
#'
#' # SummarizedExperiment identifiers use virtual dot-prefixed columns
#' mutate_col(exp, .sample = paste0("new_", .sample))
#' mutate_row(exp, .variable = paste0("new_", .variable))
#'
#' @export
mutate_col <- function(exp, ...) {
  quos <- rlang::enquos(...)
  mutate_info_data(
    exp = exp,
    quos = quos,
    info_type = "sample",
    info_field = "sample_info",
    id_column = "sample",
    matrix_dimnames_setter = function(mat, new_names) {
      colnames(mat) <- new_names
      mat
    }
  )
}


#' @rdname mutate_col
#' @export
mutate_row <- function(exp, ...) {
  quos <- rlang::enquos(...)
  mutate_info_data(
    exp = exp,
    quos = quos,
    info_type = "variable",
    info_field = "var_info",
    id_column = "variable",
    matrix_dimnames_setter = function(mat, new_names) {
      rownames(mat) <- new_names
      mat
    }
  )
}


# Internal function that handles the common logic for both mutate_col and mutate_row
mutate_info_data <- function(
  exp,
  quos,
  info_type,
  info_field,
  id_column,
  matrix_dimnames_setter
) {
  stopifnot(is_tidy_container(exp))
  id_column <- tidy_id_column(exp, id_column)

  # Get original data and mutate it
  original_data <- tidy_info_data(exp, info_field, id_column)
  new_data <- try_mutate(original_data, info_field, id_column, quos)

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
    if (.is_experiment(exp)) {
      new_expr_mat <- matrix_dimnames_setter(exp$expr_mat, new_ids)
    }
  } else {
    if (.is_experiment(exp)) {
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
try_mutate <- function(data, data_type, id_column, quos) {
  tryCatch(
    dplyr::mutate(data, !!!quos),
    error = function(e) {
      missing_col <- extract_missing_column(conditionMessage(e))
      if (!is.na(missing_col)) {
        abort_missing_tidy_column(
          missing_col,
          data_type,
          colnames(data),
          id_column
        )
      }
      stop(e)
    }
  )
}
