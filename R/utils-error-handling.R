# Error handling utilities for glyexp package

#' Find the original user function call from the call stack
#'
#' This function searches through the call stack to find calls to specified
#' functions. It's used to provide better error messages that show the correct 
#' function name rather than internal helper function names.
#'
#' @param user_functions A character vector of function names to search for
#'   in the call stack.
#' @return The first matching call found in the stack, or the result of
#'   `rlang::caller_call()` if no match is found.
#' @noRd
find_user_call <- function(user_functions) {
  # Get all calls in the stack
  calls <- sys.calls()
  
  # Look for specified functions in the call stack
  for (i in length(calls):1) {
    call_expr <- calls[[i]][[1]]
    
    # Handle different types of call expressions safely
    call_name <- tryCatch({
      if (is.symbol(call_expr)) {
        as.character(call_expr)
      } else if (is.call(call_expr)) {
        as.character(call_expr[[1]])
      } else {
        as.character(call_expr)[1]
      }
    }, error = function(e) {
      ""  # Return empty string if conversion fails
    })
    
    if (call_name %in% user_functions) {
      return(calls[[i]])
    }
  }
  
  # Fallback to caller_call if not found
  rlang::caller_call()
}

 