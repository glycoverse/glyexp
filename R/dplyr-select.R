#' Select columns of the sample or variable information tibble
#'
#' @description
#' These two functions provide a way to trimming down the sample or variable information tibble
#' of an [experiment()] to only the columns of interest.
#'
#' The same syntax as `dplyr::select()` is used.
#' For example, to get a new [experiment()] with only the "sample" and "group"
#' columns in the sample information tibble,
#' use `select_obs(exp, group)`.
#' Note that you don't need to (and you can't) explicitly select or deselect the
#'  `sample` column in `sample_info`.
#' It is automatically handled by `select_obs()`, always being selected.
#' The same applies to the `variable` column in `var_info`.
#'
#' @details
#' When using `select_var()` with `dplyr`, you may encounter package conflicts.
#' `dplyr` also has a function called `select_var()` that has been deprecated for over two years.
#' If you encounter package conflicts, use the following code to resolve them:
#'
#' ```R
#' conflicted::conflicts_prefer(glyexp::select_var)
#' ```
#'
#' @param exp An [experiment()].
#' @param ... <[`data-masking`][rlang::args_data_masking]> Column names to select.
#'   If empty, all columns except the `sample` or `variable` column will be discarded.
#'
#' @return An new [experiment()] object.
#'
#' @examples
#' toy_exp <- toy_experiment
#'
#' toy_exp_2 <- toy_exp |>
#'   select_obs(group) |>
#'   select_var(protein, peptide)
#'
#' get_sample_info(toy_exp_2)
#' get_var_info(toy_exp_2)
#'
#' @export
select_obs <- function(exp, ...) {
  select_info_data(
    exp = exp,
    info_field = "sample_info",
    id_column = "sample",
    ...
  )
}


#' @rdname select_obs
#' @export
select_var <- function(exp, ...) {
  select_info_data(
    exp = exp,
    info_field = "var_info",
    id_column = "variable",
    ...
  )
}


# Internal function that handles the common logic for both select_obs and select_var
select_info_data <- function(exp, info_field, id_column, ...) {
  checkmate::assert_class(exp, "glyexp_experiment")

  # Get original data and select it
  original_data <- exp[[info_field]]
  new_data <- select_data(original_data, info_field, id_column, ...)

  # Create new experiment object
  new_exp <- exp
  new_exp[[info_field]] <- new_data

  new_exp
}

select_data <- function(data, data_name, info_type, ...) {
  # Create a prototype (empty data frame with same structure) for validation
  prototype <- data[0, ]

  # Remove the ID column from prototype for validation
  prototype_without_id <- dplyr::select(prototype, -dplyr::all_of(info_type))

  # Try selection on the prototype first to validate
  validate_selection(prototype_without_id, data_name, info_type, ...)

  # If validation passes, perform the actual selection
  index_col <- data[[info_type]]
  new_data <- dplyr::select(data, -dplyr::all_of(info_type))
  new_data <- dplyr::select(new_data, ...)
  new_data <- dplyr::mutate(new_data, "{info_type}" := index_col, .before = 1)

  new_data
}


validate_selection <- function(prototype, data_name, info_type, ...) {
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
          cli::cli_abort(c(
            "You should not explicitly select or deselect the {.val {info_type}} column in `{data_name}`.",
            "i" = "The {.val {info_type}} column will be handled by `select_obs()` or `select_var()` automatically."
          ), call = NULL)
        } else {
          # Re-add the ID column to the prototype for accurate error message
          prototype_with_id <- dplyr::mutate(prototype, "{info_type}" := character(0), .before = 1)
          available_cols <- setdiff(colnames(prototype_with_id), info_type)
          abort_missing_column(missing_col, data_name, available_cols)
        }
      }
      stop(e)
    }
  )
}
