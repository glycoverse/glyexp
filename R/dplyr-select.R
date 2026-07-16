#' Select columns of the sample or variable information tibble
#'
#' @description
#' These two functions provide a way to trimming down the sample or variable information tibble
#' of an [experiment()] or `SummarizedExperiment` to only the columns of
#' interest.
#'
#' The same syntax as `dplyr::select()` is used.
#' For example, to get a new [experiment()] with only the "sample" and "group"
#' columns in the sample information tibble,
#' use `select_col(exp, group)`.
#' Note that you don't need to (and you can't) explicitly select or deselect the
#'  `sample` column in `sample_info`.
#' The same applies to the `variable` column in `var_info`.
#' Whatever the selection expression is, the `sample` or `variable` column will always be kept.
#'
#' @param exp An [experiment()] or `SummarizedExperiment` object.
#' @param ... <[`data-masking`][rlang::args_data_masking]> Column names to select.
#'   If empty, all columns except the `sample` or `variable` column will be discarded.
#'
#' @return An object of the same class as `exp`.
#'
#' @inheritSection mutate_col Identifier columns
#' @examples
#' toy_exp <- real_experiment
#'
#' toy_exp_2 <- toy_exp |>
#'   select_col(group) |>
#'   select_row(protein, peptide)
#'
#' toy_exp_2
#'
#' @export
select_col <- function(exp, ...) {
  select_info_data(
    exp = exp,
    info_field = "sample_info",
    id_column = "sample",
    ...
  )
}


#' @rdname select_col
#' @export
select_row <- function(exp, ...) {
  select_info_data(
    exp = exp,
    info_field = "var_info",
    id_column = "variable",
    ...
  )
}


# Internal function that handles the common logic for both select_col and select_row
select_info_data <- function(exp, info_field, id_column, ...) {
  stopifnot(is_tidy_container(exp))
  id_column <- tidy_id_column(exp, id_column)

  # Get original data and select it
  original_data <- tidy_info_data(exp, info_field, id_column)
  new_data <- select_data(original_data, info_field, id_column, ...)

  if (methods::is(exp, "SummarizedExperiment")) {
    return(update_se_info(exp, new_data, info_field, id_column))
  }

  # Create new experiment object
  new_exp <- exp
  new_exp[[info_field]] <- new_data

  new_exp
}

select_data <- function(data, data_name, info_type, ...) {
  reserved_columns <- c(info_type, tidy_position_column())
  protected_columns <- intersect(reserved_columns, colnames(data))

  # Create a prototype (empty data frame with same structure) for validation
  prototype <- data[0, ]

  # Remove protected columns from prototype for validation
  prototype_without_id <- dplyr::select(
    prototype,
    -dplyr::all_of(protected_columns)
  )

  # Try selection on the prototype first to validate
  validate_selection(
    prototype_without_id,
    data_name,
    info_type,
    id_exists = info_type %in% protected_columns,
    ...
  )

  # If validation passes, perform the actual selection
  protected_data <- dplyr::select(data, dplyr::all_of(protected_columns))
  new_data <- dplyr::select(data, -dplyr::all_of(protected_columns))
  new_data <- dplyr::select(new_data, ...)
  check_reserved_tidy_columns(new_data, reserved_columns, data_name)
  new_data <- dplyr::bind_cols(protected_data, new_data)

  new_data
}


validate_selection <- function(
  prototype,
  data_name,
  info_type,
  id_exists,
  ...
) {
  tryCatch(
    {
      # Try the selection on the prototype
      dplyr::select(prototype, ...)
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
            c(
              "You should not explicitly select or deselect the {.val {info_type}} column in `{data_name}`.",
              "i" = "The {.val {info_type}} column will be handled by `select_col()` or `select_row()` automatically."
            ),
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
