#' Filter Samples or Variables of an Experiment
#'
#' @description
#' Getting a subset of an [experiment()] by filtering samples or variables.
#'
#' The same syntax as [dplyr::filter()] is used.
#' For example, to get a subset of an experiment keeping only "HC" samples,
#' use `filter_samples(exp, group == "HC")`.
#' This actually calls `dplyr::filter()` on the sample information tibble
#' with condition `group == "HC"`,
#' and then updates the expression matrix accordingly.
#'
#' If no samples or variables are left after filtering, an error is thrown.
#'
#' @param exp An [experiment()].
#' @param ... <[`data-masking`][rlang::args_data_masking]> Expression to filter samples or variables.
#'   passed to [dplyr::filter()] internally.
#'
#' @return An new [experiment()] object.
#'
#' @examples
#' library(magrittr)
#'
#' # Create a toy experiment for demonstration
#' expr_mat <- matrix(1:25, nrow = 5)
#' colnames(expr_mat) <- paste0("S", 1:5)
#' rownames(expr_mat) <- paste0("V", 1:5)
#' sample_info <- tibble::tibble(
#'   sample = paste0("S", 1:5),
#'   group = c("A", "A", "A", "B", "B")
#' )
#' var_info <- tibble::tibble(
#'   variable = paste0("V", 1:5),
#'   type = c("X", "X", "Y", "Y", "Y")
#' )
#' exp <- experiment("my_exp", expr_mat, sample_info, var_info)
#'
#' # Filter samples
#' sub_exp_1 <- filter_samples(exp, group == "A")
#' get_sample_info(sub_exp_1)
#' get_expr_mat(sub_exp_1)
#'
#' # Filter variables
#' sub_exp_2 <- filter_variables(exp, type == "X")
#' get_var_info(sub_exp_2)
#' get_expr_mat(sub_exp_2)
#'
#' # Use pipe
#' sub_exp_3 <- exp %>%
#'   filter_samples(group == "A") %>%
#'   filter_variables(type == "X")
#' get_sample_info(sub_exp_3)
#' get_var_info(sub_exp_3)
#' get_expr_mat(sub_exp_3)
#'
#' @importFrom magrittr %>%
#'
#' @export
filter_samples <- function(exp, ...) {
  stopifnot(is_experiment(exp))
  new_sample_info <- filter_data(exp$sample_info, "sample_info", "samples", rlang::expr(filter_samples()), ...)
  new_expr_mat <- exp$expr_mat[, new_sample_info$sample, drop = FALSE]
  new_experiment(exp$name, new_expr_mat, new_sample_info, exp$var_info)
}


#' @rdname filter_samples
#' @export
filter_variables <- function(exp, ...) {
  stopifnot(is_experiment(exp))
  new_var_info <- filter_data(exp$var_info, "var_info", "variables", rlang::expr(filter_variables()), ...)
  new_expr_mat <- exp$expr_mat[new_var_info$variable, , drop = FALSE]
  new_experiment(exp$name, new_expr_mat, exp$sample_info, new_var_info)
}


filter_data <- function(data, data_type, dim_name, call, ...) {
  # data: `sample_info` or `var_info`
  # data_type: "sample_info" or "var_info", used in error messages
  # dim_name: "samples" or "variables", used in error messages
  # call: a defused call to `filter_samples()` or `filter_variables()`, used in error messages
  # ...: arguments to pass to `dplyr::filter()`
  tryCatch(
    new_data <- dplyr::filter(data, ...),
    error = function(e) {
      if (grepl("object '.*' not found", conditionMessage(e))) {
        missing_col <- stringr::str_extract(conditionMessage(e), "'(.*)' not found", group = 1)
        available_cols <- colnames(data)
        cli::cli_abort(c(
          "Column {.field {missing_col}} not found in `{data_type}`.",
          "i" = "Available columns: {.field {available_cols}}"
        ), call = call)
      } else {
        stop(e)
      }
    }
  )

  if (nrow(new_data) == 0) {
    cli::cli_abort("No {dim_name} left after filtering.", call = call)
  }

  new_data
}
