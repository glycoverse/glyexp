#' Deprecated dplyr-style aliases
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' The `_obs()` and `_var()` helpers have been renamed to `_col()` and `_row()`,
#' respectively. Use the new names in new code.
#'
#' @name deprecated-dplyr-aliases
#' @keywords internal
NULL

#' @rdname deprecated-dplyr-aliases
#' @export
arrange_obs <- function(...) {
  .deprecate_dplyr_alias("arrange_obs", "arrange_col")
  arrange_col(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
arrange_var <- function(...) {
  .deprecate_dplyr_alias("arrange_var", "arrange_row")
  arrange_row(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
filter_obs <- function(...) {
  .deprecate_dplyr_alias("filter_obs", "filter_col")
  filter_col(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
filter_var <- function(...) {
  .deprecate_dplyr_alias("filter_var", "filter_row")
  filter_row(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
mutate_obs <- function(...) {
  .deprecate_dplyr_alias("mutate_obs", "mutate_col")
  mutate_col(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
mutate_var <- function(...) {
  .deprecate_dplyr_alias("mutate_var", "mutate_row")
  mutate_row(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
rename_obs <- function(...) {
  .deprecate_dplyr_alias("rename_obs", "rename_col")
  rename_col(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
rename_var <- function(...) {
  .deprecate_dplyr_alias("rename_var", "rename_row")
  rename_row(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
select_obs <- function(...) {
  .deprecate_dplyr_alias("select_obs", "select_col")
  select_col(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
select_var <- function(...) {
  .deprecate_dplyr_alias("select_var", "select_row")
  select_row(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
slice_obs <- function(...) {
  .deprecate_dplyr_alias("slice_obs", "slice_col")
  slice_col(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
slice_var <- function(...) {
  .deprecate_dplyr_alias("slice_var", "slice_row")
  slice_row(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
slice_head_obs <- function(...) {
  .deprecate_dplyr_alias("slice_head_obs", "slice_head_col")
  slice_head_col(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
slice_head_var <- function(...) {
  .deprecate_dplyr_alias("slice_head_var", "slice_head_row")
  slice_head_row(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
slice_tail_obs <- function(...) {
  .deprecate_dplyr_alias("slice_tail_obs", "slice_tail_col")
  slice_tail_col(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
slice_tail_var <- function(...) {
  .deprecate_dplyr_alias("slice_tail_var", "slice_tail_row")
  slice_tail_row(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
slice_sample_obs <- function(...) {
  .deprecate_dplyr_alias("slice_sample_obs", "slice_sample_col")
  slice_sample_col(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
slice_sample_var <- function(...) {
  .deprecate_dplyr_alias("slice_sample_var", "slice_sample_row")
  slice_sample_row(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
slice_max_obs <- function(...) {
  .deprecate_dplyr_alias("slice_max_obs", "slice_max_col")
  slice_max_col(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
slice_max_var <- function(...) {
  .deprecate_dplyr_alias("slice_max_var", "slice_max_row")
  slice_max_row(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
slice_min_obs <- function(...) {
  .deprecate_dplyr_alias("slice_min_obs", "slice_min_col")
  slice_min_col(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
slice_min_var <- function(...) {
  .deprecate_dplyr_alias("slice_min_var", "slice_min_row")
  slice_min_row(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
left_join_obs <- function(...) {
  .deprecate_dplyr_alias("left_join_obs", "left_join_col")
  left_join_col(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
left_join_var <- function(...) {
  .deprecate_dplyr_alias("left_join_var", "left_join_row")
  left_join_row(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
inner_join_obs <- function(...) {
  .deprecate_dplyr_alias("inner_join_obs", "inner_join_col")
  inner_join_col(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
inner_join_var <- function(...) {
  .deprecate_dplyr_alias("inner_join_var", "inner_join_row")
  inner_join_row(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
semi_join_obs <- function(...) {
  .deprecate_dplyr_alias("semi_join_obs", "semi_join_col")
  semi_join_col(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
semi_join_var <- function(...) {
  .deprecate_dplyr_alias("semi_join_var", "semi_join_row")
  semi_join_row(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
anti_join_obs <- function(...) {
  .deprecate_dplyr_alias("anti_join_obs", "anti_join_col")
  anti_join_col(...)
}

#' @rdname deprecated-dplyr-aliases
#' @export
anti_join_var <- function(...) {
  .deprecate_dplyr_alias("anti_join_var", "anti_join_row")
  anti_join_row(...)
}
