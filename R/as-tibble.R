#' Convert an Experiment to a Tibble
#'
#' @description
#' Convert an experiment object to a tibble of "tidy" format.
#' That is, each row is a unique combination of "sample" and "variable",
#' with the observation (the abundance) in the "value" column.
#' Additional columns in the sample and variable information are included.
#' This format is also known as the "long" format.
#'
#' Usually you don't want all columns in the sample information or variable information
#' tibbles to be included in the output tibble,
#' as this will make the output tibble very "wide".
#' You can specify which columns to include in the output tibble
#' by passing the column names to the `sample_cols` and `var_cols` arguments.
#' <[`data-masking`][rlang::args_data_masking]> syntax is used here.
#' By default, all columns are included.
#'
#' @param x An [experiment()].
#' @param sample_cols <[`data-masking`][rlang::args_data_masking]> Columns to include from the sample information tibble.
#' @param var_cols <[`data-masking`][rlang::args_data_masking]> Columns to include from the variable information tibble.
#' @param ... Ignored.
#'
#' @return A tibble.
#'
#' @examples
#' library(tibble)
#'
#' # Create a toy experiment for demonstration
#' toy_exp <- toy_experiment()
#' toy_exp
#'
#' # Convert the experiment to a tibble
#' as_tibble(toy_exp)
#'
#' # specify columns to include
#' as_tibble(toy_exp, sample_cols = group, var_cols = c(protein, peptide))
#'
#' @importFrom tibble as_tibble
#' @export
as_tibble.glyexp_experiment <- function(
  x,
  sample_cols = tidyselect::everything(),
  var_cols = tidyselect::everything(),
  ...
) {
  stopifnot(is_experiment(x))
  # Convert the expression matrix to a long format tibble
  tb <- tibble::rownames_to_column(as.data.frame(x$expr_mat), "variable")
  tb <- tibble::as_tibble(tb)
  tb <- tidyr::pivot_longer(tb, -variable, names_to = "sample", values_to = "value")
  # Join with sample_info and var_info
  sub_sample_info <- select_data(x$sample_info, "sample_info", "sample", {{ sample_cols }})
  sub_var_info <- select_data(x$var_info, "var_info", "variable", {{ var_cols }})
  tb <- dplyr::left_join(tb, sub_sample_info, by = "sample")
  tb <- dplyr::left_join(tb, sub_var_info, by = "variable")
  # Reorder columns: sample, sample fields, variable, variable fields, value
  sample_fields <- setdiff(colnames(sub_sample_info), "sample")
  var_fields <- setdiff(colnames(sub_var_info), "variable")
  tb <- dplyr::select(
    tb,
    sample, tidyselect::all_of(sample_fields),
    variable, tidyselect::all_of(var_fields),
    value
    )
  tb
}

# Dismiss the "no visible binding" warning of R CMD check
utils::globalVariables(c("sample", "variable", "value"))
