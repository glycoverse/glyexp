#' Signal use of a deprecated experiment API
#'
#' @param what The deprecated API name.
#' @param with The replacement API name, if there is a direct replacement.
#' @param details Additional migration guidance.
#' @param user_env The user environment recorded on the warning.
#' @noRd
.deprecate_experiment <- function(
  what,
  with = NULL,
  details = "Create a GlycomicSE or GlycoproteomicSE object instead.",
  user_env = rlang::caller_env(2)
) {
  if (
    requireNamespace("testthat", quietly = TRUE) &&
      testthat::is_testing() &&
      !isTRUE(getOption("glyexp.expect_deprecation"))
  ) {
    return(invisible())
  }

  lifecycle::deprecate_warn(
    "0.16.0",
    what,
    with = with,
    details = details,
    user_env = user_env
  )
}
