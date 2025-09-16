#' Split an Experiment
#'
#' Devides an [experiment()] into a list of [experiment()] objects by `f`.
#'
#' @param x An [experiment()].
#' @param f <[`data-masking`][rlang::args_data_masking]>
#'   A column in `var_info` or `sample_info` that `as.factor(f)`defines the grouping.
#' @param drop Logical indicating if levels that do not occur should be dropped. Defaults to FALSE.
#' @param where Where to find the column, "var_info" or "sample_info".
#' @param ... Ignored
#'
#' @returns A named list of [experiment()] objects.
#'
#' @examples
#' split(toy_experiment, group, where = "sample_info")
#'
#' @export
split.glyexp_experiment <- function(x, f, drop = FALSE, where = "var_info", ...) {
  checkmate::assert_choice(where, c("var_info", "sample_info"))
  checkmate::assert_flag(drop)

  # Evaluate f
  f <- rlang::ensym(f)
  if (!rlang::as_name(f) %in% colnames(x[[where]])) {
    cli::cli_abort(c(
      "Column {.field {rlang::as_name(f)}} not found in {.field {where}}",
      i = "Available columns: {.field {colnames(x[[where]])}}"
    ))
  }
  f <- as.factor(eval(f, x[[where]]))
  if (drop) {
    f <- droplevels(f)
  }

  # Perform splitting
  info <- x[[where]]
  index_col <- if (where == "var_info") "variable" else "sample"
  info_splits <- split(info, f)
  join_func <- if (where == "var_info") semi_join_var else semi_join_obs
  purrr::map(info_splits, ~ join_func(x, .x, by = index_col))
}
