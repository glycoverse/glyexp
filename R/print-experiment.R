# Helper function to get short type abbreviations like tibble
get_col_type <- function(col) {
  vctrs::vec_ptype_abbr(col)
}

# Helper function to format field names with types
format_fields_with_types <- function(df, exclude_cols = character()) {
  field_cols <- setdiff(colnames(df), exclude_cols)
  if (length(field_cols) == 0) {
    return("none")
  }

  formatted_fields <- purrr::map_chr(field_cols, function(col_name) {
    col_type <- get_col_type(df[[col_name]])
    paste0("{.field ", col_name, "} {.cls ", col_type, "}")
  })

  paste(formatted_fields, collapse = ", ")
}

#' @export
print.glyexp_experiment <- function(x, ...) {
  cli::cli_h1("Experiment")
  cli::cli_alert_info("Expression matrix: {.val {ncol(x$expr_mat)}} samples, {.val {nrow(x$expr_mat)}} variables")

  # Format sample information fields with types
  sample_msg <- paste0("Sample information fields: ", format_fields_with_types(x$sample_info, "sample"))
  cli::cli_alert_info(sample_msg)

  # Format variable information fields with types
  var_msg <- paste0("Variable information fields: ", format_fields_with_types(x$var_info, "variable"))
  cli::cli_alert_info(var_msg)
}
