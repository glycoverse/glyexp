#' Store experiment deprecation state
#'
#' This environment records whether the current R session has already received
#' the experiment-container deprecation warning.
#'
#' @noRd
.experiment_deprecation_state <- new.env(parent = emptyenv())

#' Signal use of a deprecated experiment API
#'
#' @param what Ignored. Retained to keep all legacy entry points routed through
#'   this helper.
#' @param with Ignored.
#' @param details Ignored.
#' @param user_env The user environment recorded on the warning.
#' @noRd
.deprecate_experiment <- function(
  what,
  with = NULL,
  details = "Create a GlycomicSE or GlycoproteomicSE object instead.",
  user_env = rlang::caller_env(2)
) {
  if (isTRUE(.experiment_deprecation_state$warned)) {
    return(invisible())
  }

  if (
    requireNamespace("testthat", quietly = TRUE) &&
      testthat::is_testing() &&
      !isTRUE(getOption("glyexp.expect_deprecation"))
  ) {
    return(invisible())
  }

  .experiment_deprecation_state$warned <- TRUE

  lifecycle::deprecate_warn(
    "0.16.0",
    "experiment()",
    details = paste(
      "Use GlycomicSE() or GlycoproteomicSE() containers with",
      "SummarizedExperiment or tidySummarizedExperiment operations instead."
    ),
    id = "glyexp-experiment-container",
    user_env = user_env
  )
}
