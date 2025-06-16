# Error handling utilities for glyexp package

#' Find the original user function call from the call stack
#'
#' This function searches through the call stack to find calls to user-facing
#' functions like mutate_samples, select_samples, etc. It's used to provide
#' better error messages that show the correct function name rather than
#' internal helper function names.
#'
#' @param user_functions A character vector of function names to search for
#'   in the call stack. Defaults to common dplyr-style functions in glyexp.
#' @return The first matching call found in the stack, or the result of
#'   `rlang::caller_call()` if no match is found.
#' @keywords internal
find_user_call <- function(user_functions = c(
  "mutate_samples", "mutate_variables",
  "select_samples", "select_variables", 
  "rename_samples", "rename_variables",
  "filter_samples", "filter_variables"
)) {
  # Get all calls in the stack
  calls <- sys.calls()
  
  # Look for user-facing functions in the call stack
  for (i in length(calls):1) {
    call_name <- as.character(calls[[i]][[1]])[1]  # Take only the first element
    if (call_name %in% user_functions) {
      return(calls[[i]])
    }
  }
  
  # Fallback to caller_call if not found
  rlang::caller_call()
}

 