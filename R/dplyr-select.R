#' Select Columns of the Sample or Variable Information Tibble
#'
#' @description
#' These two functions provide a way to trimming down the sample or variable information tibble
#' of an [experiment()] to only the columns of interest.
#'
#' The same syntax as `dplyr::select()` is used.
#' For example, to get a new [experiment()] with only the "sample" and "group"
#' columns in the sample information tibble,
#' use `select_samples(exp, group)`.
#' Note that you don't need to (and you can't) explicitly select or deselect the
#'  `sample` column in `sample_info`.
#' It is automatically handled by `select_samples()`, always being selected.
#' The same applies to the `variable` column in `var_info`.
#'
#' @param exp An [experiment()].
#' @param ... <[`data-masking`][rlang::args_data_masking]> Column names to select.
#'   If empty, all columns except the `sample` or `variable` column will be discarded.
#'
#' @return An new [experiment()] object.
#'
#' @examples
#' library(magrittr)
#'
#' toy_exp <- toy_experiment()
#'
#' toy_exp_2 <- toy_exp %>%
#'   select_samples(group) %>%
#'   select_variables(protein, peptide)
#'
#' get_sample_info(toy_exp_2)
#' get_var_info(toy_exp_2)
#'
#' @export
select_samples <- function(exp, ...) {
  select_info_data(
    exp = exp,
    info_field = "sample_info",
    id_column = "sample",
    ...
  )
}


#' @rdname select_samples
#' @export
select_variables <- function(exp, ...) {
  select_info_data(
    exp = exp,
    info_field = "var_info",
    id_column = "variable",
    ...
  )
}


# Internal function that handles the common logic for both select_samples and select_variables
select_info_data <- function(exp, info_field, id_column, ...) {
  stopifnot(class(exp) == "glyexp_experiment")
  
  # Get original data and select it
  original_data <- exp[[info_field]]
  new_data <- select_data(original_data, info_field, id_column, ...)
  
  # Create new experiment object
  new_exp <- exp
  new_exp[[info_field]] <- new_data
  
  new_exp
}


#' @importFrom rlang `:=`
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
      if (grepl("Column `.*` doesn't exist", conditionMessage(e))) {
        missing_col <- stringr::str_extract(conditionMessage(e), "Column `(.*)` doesn't exist", group = 1)
        user_call <- find_user_call()
        user_fn_name <- as.character(user_call[[1]])
        if (missing_col == info_type) {
          cli::cli_abort(c(
            "You should not explicitly select or deselect the {.val {info_type}} column in `{data_name}`.",
            "i" = "The {.val {info_type}} column will be handled by `{user_fn_name}()` automatically."
          ), call = user_call)
        } else {
          # Re-add the ID column to the prototype for accurate error message
          prototype_with_id <- dplyr::mutate(prototype, "{info_type}" := character(0), .before = 1)
          available_cols <- setdiff(colnames(prototype_with_id), info_type)
          cli::cli_abort(c(
            "Column {.field {missing_col}} not found in `{data_name}`.",
            "i" = "Available columns: {.field {available_cols}}"
          ), call = user_call)
        }
      } else {
        stop(e)
      }
    }
  )
}
