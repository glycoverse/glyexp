#' Rename columns in the sample or variable information tibble
#'
#' @description
#' These two functions provide a way to rename columns in the sample or variable
#' information of an [experiment()] or `SummarizedExperiment`.
#'
#' The same syntax as `dplyr::rename()` is used.
#' For example, to rename the "group" column in the sample information tibble to "condition",
#' use `rename_col(exp, condition = group)`.
#' Note that you can't rename the "sample" column in the sample information tibble,
#' as well as the "variable" column in the variable information tibble.
#' These two columns are used to link the sample or variable information tibble
#' to the expression matrix.
#'
#' @param exp An [experiment()] or `SummarizedExperiment` object.
#' @param ... <[`data-masking`][rlang::args_data_masking]> Name pairs to rename.
#'   Use `new_name = old_name` to rename columns.
#'
#' @returns An object of the same class as `exp`.
#'
#' @inheritSection mutate_col Identifier columns
#' @examples
#' toy_exp <- real_experiment
#' toy_exp
#'
#' # Rename columns in sample information tibble
#' rename_col(toy_exp, condition = group)
#'
#' # Rename columns in variable information tibble
#' rename_row(toy_exp, composition = glycan_composition)
#'
#' @export
rename_col <- function(exp, ...) {
  rename_info_data(
    exp = exp,
    info_field = "sample_info",
    id_column = "sample",
    ...
  )
}

#' @rdname rename_col
#' @export
rename_row <- function(exp, ...) {
  rename_info_data(
    exp = exp,
    info_field = "var_info",
    id_column = "variable",
    ...
  )
}

# Internal function that handles the common logic for both rename_col and rename_row
rename_info_data <- function(exp, info_field, id_column, ...) {
  stopifnot(is_tidy_container(exp))
  id_column <- tidy_id_column(exp, id_column)

  # Get original data and rename it
  original_data <- tidy_info_data(exp, info_field, id_column)
  new_data <- rename_data(original_data, info_field, id_column, ...)

  if (methods::is(exp, "SummarizedExperiment")) {
    return(update_se_info(exp, new_data, info_field, id_column))
  }

  # Create new experiment object
  new_exp <- exp
  new_exp[[info_field]] <- new_data

  new_exp
}

rename_data <- function(data, data_name, info_type, ...) {
  reserved_columns <- c(info_type, tidy_position_column())
  protected_columns <- intersect(reserved_columns, colnames(data))

  # Create a prototype (empty data frame with same structure) for validation
  prototype <- data[0, ]

  # Remove protected columns from prototype for validation
  prototype_without_id <- dplyr::select(
    prototype,
    -dplyr::all_of(protected_columns)
  )

  # Try renaming on the prototype first to validate
  validate_rename(
    prototype_without_id,
    data_name,
    info_type,
    id_exists = info_type %in% protected_columns,
    ...
  )

  # If validation passes, perform the actual renaming
  protected_data <- dplyr::select(data, dplyr::all_of(protected_columns))
  new_data <- dplyr::select(data, -dplyr::all_of(protected_columns))
  new_data <- dplyr::rename(new_data, ...)
  check_reserved_tidy_columns(new_data, reserved_columns, data_name)
  new_data <- dplyr::bind_cols(protected_data, new_data)

  new_data
}

validate_rename <- function(prototype, data_name, info_type, id_exists, ...) {
  tryCatch(
    {
      # Try the renaming on the prototype
      dplyr::rename(prototype, ...)
      invisible(TRUE)
    },
    error = function(e) {
      missing_col <- extract_missing_column(conditionMessage(e))
      if (!is.na(missing_col)) {
        if (missing_col == info_type) {
          if (!id_exists && info_type %in% c(".sample", ".variable")) {
            abort_missing_tidy_identifier(info_type)
          }
          cli::cli_abort(
            "You could not rename the {.val {info_type}} column in `{data_name}`.",
            call = NULL
          )
        } else {
          # Re-add the ID column to the prototype for accurate error message
          prototype_with_id <- dplyr::mutate(
            prototype,
            "{info_type}" := character(0),
            .before = 1
          )
          available_cols <- setdiff(colnames(prototype_with_id), info_type)
          abort_missing_column(missing_col, data_name, available_cols)
        }
      }
      stop(e)
    }
  )
}
