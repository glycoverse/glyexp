extract_missing_column <- function(error_msg) {
  patterns <- c(
    "object '([^']+)' not found",
    "Column `([^`]+)` doesn't exist"
  )

  for (pattern in patterns) {
    match <- stringr::str_match(error_msg, pattern)
    if (!is.na(match[1, 2])) {
      return(match[1, 2])
    }
  }

  NA_character_
}

abort_missing_column <- function(missing_col, data_name, available_cols) {
  cli::cli_abort(c(
    "Column {.field {missing_col}} not found in `{data_name}`.",
    "i" = "Available columns: {.field {available_cols}}"
  ), call = NULL)
}
