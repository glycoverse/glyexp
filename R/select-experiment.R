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
  stopifnot(class(exp) == "glyexp_experiment")
  sample_col <- exp$sample_info$sample
  new_sample_info <- dplyr::select(exp$sample_info, -sample)
  new_sample_info <- try_select(new_sample_info, "sample_info", "sample", rlang::expr(select_samples()), ...)
  new_sample_info <- dplyr::mutate(new_sample_info, sample = sample_col, .before = 1)
  new_experiment(exp$name, exp$expr_mat, new_sample_info, exp$var_info)
}


#' @rdname select_samples
#' @export
select_variables <- function(exp, ...) {
  stopifnot(class(exp) == "glyexp_experiment")
  var_col <- exp$var_info$variable
  new_var_info <- dplyr::select(exp$var_info, -variable)
  new_var_info <- try_select(new_var_info, "var_info", "variable", rlang::expr(select_variables()), ...)
  new_var_info <- dplyr::mutate(new_var_info, variable = var_col, .before = 1)
  new_experiment(exp$name, exp$expr_mat, exp$sample_info, new_var_info)
}


try_select <- function(data, data_name, info_type, call, ...) {
  tryCatch(
    dplyr::select(data, ...),
    error = function(e) {
      if (grepl("Column `.*` doesn't exist", conditionMessage(e))) {
        missing_col <- stringr::str_extract(conditionMessage(e), "Column `(.*)` doesn't exist", group = 1)
        if (missing_col == info_type) {
          cli::cli_abort(c(
            "You should not explicitly select or deselect the {.val {info_type}} column in `{data_name}`.",
            "i" = "The {.val {info_type}} column will be handled by `{call}` automatically."
          ), call = call)
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
