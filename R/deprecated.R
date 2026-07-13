#' Warn about a deprecated legacy experiment API
#'
#' @param what A precise description of the deprecated function or behavior.
#' @returns `NULL`, invisibly.
#' @noRd
.deprecate_experiment_api <- function(what) {
  lifecycle::deprecate_warn(
    when = "0.16.0",
    what = what,
    details = paste0(
      "Use `GlycomicSE()` or `GlycoproteomicSE()` as the default data ",
      "container."
    ),
    env = rlang::caller_env(),
    user_env = rlang::caller_env(2)
  )
}
