#' Rename Columns in the Sample or Variable Information Tibble
#'
#' @description
#' These two functions provide a way to rename columns in the sample or variable
#' information tibble of an [experiment()].
#'
#' The same syntax as `dplyr::rename()` is used.
#' For example, to rename the "group" column in the sample information tibble to "condition",
#' use `rename_samples(exp, condition = group)`.
#' Note that you can't rename the "sample" column in the sample information tibble,
#' as well as the "variable" column in the variable information tibble.
#' These two columns are used to link the sample or variable information tibble
#' to the expression matrix.
#'
#' @param exp An [experiment()].
#' @param ... <[`data-masking`][rlang::args_data_masking]> Name pairs to rename.
#'   Use `new_name = old_name` to rename columns.
#'
#' @returns An new [experiment()] object.
#'
#' @examples
#' toy_exp <- toy_experiment()
#' toy_exp
#'
#' # Rename columns in sample information tibble
#' rename_samples(toy_exp, condition = group)
#'
#' # Rename columns in variable information tibble
#' rename_variables(toy_exp, composition = glycan_composition)
#'
#' @export
rename_samples <- function(exp, ...) {
  stopifnot(is_experiment(exp))
  new_sample_info <- rename_data(exp$sample_info, "sample_info", "sample", ...)
  new_exp <- exp
  new_exp$sample_info <- new_sample_info
  new_exp
}


#' @rdname rename_samples
#' @export
rename_variables <- function(exp, ...) {
  stopifnot(is_experiment(exp))
  new_var_info <- rename_data(exp$var_info, "var_info", "variable", ...)
  new_exp <- exp
  new_exp$var_info <- new_var_info
  new_exp
}


#' @importFrom rlang `:=`
rename_data <- function(data, data_name, info_type, ...) {
  index_col <- data[[info_type]]
  new_data <- dplyr::select(data, -dplyr::all_of(info_type))
  if (info_type == "sample") {
    call <- rlang::expr(select_samples())
  } else {
    call <- rlang::expr(select_variables())
  }
  new_data <- try_rename(new_data, data_name, info_type, call, ...)
  new_data <- dplyr::mutate(new_data, "{info_type}" := index_col, .before = 1)
}


try_rename <- function(data, data_name, info_type, call, ...) {
  tryCatch(
    dplyr::rename(data, ...),
    error = function(e) {
      if (grepl("Column `.*` doesn't exist", conditionMessage(e))) {
        missing_col <- stringr::str_extract(conditionMessage(e), "Column `(.*)` doesn't exist", group = 1)
        if (missing_col == info_type) {
          cli::cli_abort(
            "You could not rename the {.val {info_type}} column in `{data_name}`.",
            call = call
          )
        } else {
          available_cols <- colnames(data)
          cli::cli_abort(c(
            "Column {.field {missing_col}} not found in `{data_name}`.",
            "i" = "Available columns: {.field {available_cols}}"
          ), call = call)
        }
      } else {
        stop(e)
      }
    }
  )
}
