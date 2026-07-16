#' Join data to sample or variable information
#'
#' @description
#' These functions allow you to join additional data to the sample information
#' or variable information of an [experiment()] or `SummarizedExperiment`.
#' They work similarly to
#' [dplyr::left_join()], [dplyr::inner_join()], [dplyr::semi_join()], and
#' [dplyr::anti_join()], while keeping assay dimensions synchronized with the
#' joined metadata.
#'
#' After joining, the assay dimensions are automatically updated to reflect any
#' changes in the number of samples or variables.
#'
#' **Important Notes:**
#' - The `relationship` parameter is locked to "many-to-one" to ensure that
#'   the number of observations never increases, which would violate the
#'   experiment object assumptions.
#' - `right_join()` and `full_join()` are not supported as they could add
#'   new observations to the experiment.
#'
#' @param exp An [experiment()] or `SummarizedExperiment` object.
#' @param y A data frame to join to `sample_info` or `var_info`.
#' @param by A join specification created with [dplyr::join_by()], or a
#'   character vector of variables to join by. See [dplyr::left_join()] for details.
#' @param ... Other arguments passed to the underlying dplyr join function,
#'   except `relationship` which is locked to "many-to-one".
#'
#' @return An object of the same class as `exp`, with updated sample or variable
#'   information.
#' @inheritSection mutate_col Identifier columns
#' @seealso [dplyr::left_join()]
#' @export
left_join_col <- function(exp, y, by = NULL, ...) {
  join_info_data(
    exp = exp,
    y = y,
    by = by,
    join_type = "left",
    info_field = "sample_info",
    id_column = "sample",
    dim_name = "samples",
    matrix_updater = function(mat, ids) mat[, ids, drop = FALSE],
    ...
  )
}

#' @rdname left_join_col
#' @export
inner_join_col <- function(exp, y, by = NULL, ...) {
  join_info_data(
    exp = exp,
    y = y,
    by = by,
    join_type = "inner",
    info_field = "sample_info",
    id_column = "sample",
    dim_name = "samples",
    matrix_updater = function(mat, ids) mat[, ids, drop = FALSE],
    ...
  )
}

#' @rdname left_join_col
#' @export
semi_join_col <- function(exp, y, by = NULL, ...) {
  join_info_data(
    exp = exp,
    y = y,
    by = by,
    join_type = "semi",
    info_field = "sample_info",
    id_column = "sample",
    dim_name = "samples",
    matrix_updater = function(mat, ids) mat[, ids, drop = FALSE],
    ...
  )
}

#' @rdname left_join_col
#' @export
anti_join_col <- function(exp, y, by = NULL, ...) {
  join_info_data(
    exp = exp,
    y = y,
    by = by,
    join_type = "anti",
    info_field = "sample_info",
    id_column = "sample",
    dim_name = "samples",
    matrix_updater = function(mat, ids) mat[, ids, drop = FALSE],
    ...
  )
}

#' @rdname left_join_col
#' @export
left_join_row <- function(exp, y, by = NULL, ...) {
  join_info_data(
    exp = exp,
    y = y,
    by = by,
    join_type = "left",
    info_field = "var_info",
    id_column = "variable",
    dim_name = "variables",
    matrix_updater = function(mat, ids) mat[ids, , drop = FALSE],
    ...
  )
}

#' @rdname left_join_col
#' @export
inner_join_row <- function(exp, y, by = NULL, ...) {
  join_info_data(
    exp = exp,
    y = y,
    by = by,
    join_type = "inner",
    info_field = "var_info",
    id_column = "variable",
    dim_name = "variables",
    matrix_updater = function(mat, ids) mat[ids, , drop = FALSE],
    ...
  )
}

#' @rdname left_join_col
#' @export
semi_join_row <- function(exp, y, by = NULL, ...) {
  join_info_data(
    exp = exp,
    y = y,
    by = by,
    join_type = "semi",
    info_field = "var_info",
    id_column = "variable",
    dim_name = "variables",
    matrix_updater = function(mat, ids) mat[ids, , drop = FALSE],
    ...
  )
}

#' @rdname left_join_col
#' @export
anti_join_row <- function(exp, y, by = NULL, ...) {
  join_info_data(
    exp = exp,
    y = y,
    by = by,
    join_type = "anti",
    info_field = "var_info",
    id_column = "variable",
    dim_name = "variables",
    matrix_updater = function(mat, ids) mat[ids, , drop = FALSE],
    ...
  )
}

# Internal function that handles the common logic for all join operations
join_info_data <- function(
  exp,
  y,
  by,
  join_type,
  info_field,
  id_column,
  dim_name,
  matrix_updater,
  ...
) {
  # Input validation
  stopifnot(is_tidy_container(exp))
  id_column <- tidy_id_column(exp, id_column)

  # Check if user tries to specify relationship parameter
  dots <- list(...)
  if ("relationship" %in% names(dots)) {
    cli::cli_abort(
      c(
        "The {.arg relationship} parameter is locked to \"many-to-one\".",
        "i" = "This ensures that the number of {dim_name} never increases, which would violate experiment object assumptions."
      ),
      call = NULL
    )
  }

  # Get original data
  original_data <- tidy_info_data(exp, info_field, id_column)

  # Perform the join
  # Only apply relationship constraint for joins that can increase row count
  if (join_type %in% c("left", "inner")) {
    new_data <- try_join(
      x = original_data,
      y = y,
      by = by,
      join_type = join_type,
      info_field = info_field,
      id_column = id_column,
      dim_name = dim_name,
      relationship = "many-to-one",
      ...
    )
  } else {
    # semi_join and anti_join don't add columns or increase row counts
    new_data <- try_join(
      x = original_data,
      y = y,
      by = by,
      join_type = join_type,
      info_field = info_field,
      id_column = id_column,
      dim_name = dim_name,
      ...
    )
  }

  if (methods::is(exp, "SummarizedExperiment")) {
    if (
      !id_column %in% colnames(original_data) &&
        id_column %in% colnames(new_data)
    ) {
      data_name <- if (info_field == "sample_info") {
        "colData(exp)"
      } else {
        "rowData(exp)"
      }
      abort_reserved_tidy_column(id_column, data_name)
    }
    return(update_se_info(exp, new_data, info_field, id_column, subset = TRUE))
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

# Wrapper for dplyr join functions that provides better error messages
try_join <- function(
  x,
  y,
  by,
  join_type,
  info_field,
  id_column,
  dim_name,
  ...
) {
  # Select the appropriate dplyr join function
  join_func <- switch(
    join_type,
    "left" = dplyr::left_join,
    "inner" = dplyr::inner_join,
    "semi" = dplyr::semi_join,
    "anti" = dplyr::anti_join,
    stop("Unknown join type: ", join_type)
  )

  tryCatch(
    join_func(x, y, by = by, ...),
    error = function(e) {
      error_msg <- conditionMessage(e)

      if (
        !id_column %in% colnames(x) &&
          grepl(paste0("`", id_column, "`"), error_msg, fixed = TRUE)
      ) {
        abort_missing_tidy_identifier(id_column)
      }

      # Handle common join errors with better messages
      if (grepl("Join columns must be present", error_msg)) {
        cli::cli_abort(
          c(
            "Join columns are missing.",
            "i" = "Check that the join columns exist in both `{info_field}` and the data frame to join.",
            "i" = "Available columns in `{info_field}`: {.field {colnames(x)}}",
            "i" = "Available columns in data frame to join: {.field {colnames(y)}}"
          ),
          call = NULL
        )
      } else if (grepl("relationship", error_msg)) {
        cli::cli_abort(
          c(
            "Join relationship constraint violated.",
            "i" = "The join must be \"many-to-one\" to prevent adding new {dim_name} to the experiment.",
            "i" = "Check for duplicate keys in the data frame being joined."
          ),
          call = NULL
        )
      } else {
        # Re-throw original error with better call context
        cli::cli_abort(error_msg, call = NULL)
      }
    }
  )
}
