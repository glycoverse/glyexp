#' Join data to sample or variable information
#'
#' @description
#' These functions allow you to join additional data to the sample information
#' or variable information of an [experiment()]. They work similarly to
#' [dplyr::left_join()], [dplyr::inner_join()], [dplyr::semi_join()], and
#' [dplyr::anti_join()], but are designed to work with experiment objects.
#'
#' After joining, the `expr_mat` is automatically updated to reflect any
#' changes in the number of samples or variables.
#'
#' **Important Notes:**
#' - The `relationship` parameter is locked to "many-to-one" to ensure that
#'   the number of observations never increases, which would violate the
#'   experiment object assumptions.
#' - `right_join()` and `full_join()` are not supported as they could add
#'   new observations to the experiment.
#'
#' @param exp An [experiment()].
#' @param y A data frame to join to `sample_info` or `var_info`.
#' @param by A join specification created with [dplyr::join_by()], or a
#'   character vector of variables to join by. See [dplyr::left_join()] for details.
#' @param ... Other arguments passed to the underlying dplyr join function,
#'   except `relationship` which is locked to "many-to-one".
#'
#' @return A new [experiment()] object with updated sample or variable information.
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#'
#' # Create a toy experiment
#' exp <- toy_experiment
#'
#' # Create additional sample information to join
#' extra_sample_info <- tibble(
#'   sample = c("S1", "S2", "S3", "S4"),
#'   age = c(25, 30, 35, 40),
#'   treatment = c("A", "B", "A", "B")
#' )
#'
#' # Left join to sample information
#' exp_with_extra <- left_join_obs(exp, extra_sample_info, by = "sample")
#' get_sample_info(exp_with_extra)
#'
#' # Inner join (only keeps matching samples)
#' exp_inner <- inner_join_obs(exp, extra_sample_info, by = "sample")
#' get_sample_info(exp_inner)
#' get_expr_mat(exp_inner) # Note: expr_mat is updated too
#'
#' # Create additional variable information to join
#' extra_var_info <- tibble(
#'   protein = c("P1", "P2", "P3"),
#'   pathway = c("A", "B", "A"),
#'   importance = c(0.8, 0.6, 0.9)
#' )
#'
#' # Left join to variable information
#' exp_with_var_extra <- left_join_var(exp, extra_var_info, by = "protein")
#' get_var_info(exp_with_var_extra)
#'
#' @export
left_join_obs <- function(exp, y, by = NULL, ...) {
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

#' @rdname left_join_obs
#' @export
inner_join_obs <- function(exp, y, by = NULL, ...) {
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

#' @rdname left_join_obs
#' @export
semi_join_obs <- function(exp, y, by = NULL, ...) {
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

#' @rdname left_join_obs
#' @export
anti_join_obs <- function(exp, y, by = NULL, ...) {
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

#' @rdname left_join_obs
#' @export
left_join_var <- function(exp, y, by = NULL, ...) {
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

#' @rdname left_join_obs
#' @export
inner_join_var <- function(exp, y, by = NULL, ...) {
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

#' @rdname left_join_obs
#' @export
semi_join_var <- function(exp, y, by = NULL, ...) {
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

#' @rdname left_join_obs
#' @export
anti_join_var <- function(exp, y, by = NULL, ...) {
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
join_info_data <- function(exp, y, by, join_type, info_field, id_column, dim_name, matrix_updater, ...) {
  # Input validation
  stopifnot(is_experiment(exp))

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
  original_data <- exp[[info_field]]

  # Perform the join
  # Only apply relationship constraint for joins that can increase row count
  if (join_type %in% c("left", "inner")) {
    new_data <- try_join(
      x = original_data,
      y = y,
      by = by,
      join_type = join_type,
      info_field = info_field,
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
      dim_name = dim_name,
      ...
    )
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
try_join <- function(x, y, by, join_type, info_field, dim_name, ...) {
  # Select the appropriate dplyr join function
  join_func <- switch(join_type,
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

      # Handle common join errors with better messages
      if (grepl("Join columns must be present", error_msg)) {
        cli::cli_abort(c(
          "Join columns are missing.",
          "i" = "Check that the join columns exist in both `{info_field}` and the data frame to join.",
          "i" = "Available columns in `{info_field}`: {.field {colnames(x)}}",
          "i" = "Available columns in data frame to join: {.field {colnames(y)}}"
        ), call = NULL)
      } else if (grepl("relationship", error_msg)) {
        cli::cli_abort(c(
          "Join relationship constraint violated.",
          "i" = "The join must be \"many-to-one\" to prevent adding new {dim_name} to the experiment.",
          "i" = "Check for duplicate keys in the data frame being joined."
        ), call = NULL)
      } else {
        # Re-throw original error with better call context
        cli::cli_abort(error_msg, call = NULL)
      }
    }
  )
}
