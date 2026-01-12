#' Replace <site> token in format string with computed site values
#' @keywords internal
.resolve_site_token <- function(var_info, format, site_aa_pos) {
  if (stringr::str_detect(format, "<site>")) {
    if (length(site_aa_pos) != nrow(var_info)) {
      cli::cli_abort("Length of site_aa_pos must match number of variables.")
    }
    # Replace <site> with site_aa_pos values
    var_info$site_aa_pos <- site_aa_pos
    # Replace <site> with placeholder that can be resolved by glue
    temp_format <- stringr::str_replace_all(format, "<site>", "{site_aa_pos}")
    # Use glue_data but handle unresolvable placeholders by replacing them first
    # Find all {xxx} patterns that are not site_aa_pos and not in var_info
    other_placeholders <- stringr::str_extract_all(temp_format, "\\{[^}]+\\}")[[1]]
    other_placeholders <- other_placeholders[other_placeholders != "{site_aa_pos}"]
    # Only protect placeholders that are NOT in var_info columns
    var_names <- names(var_info)
    unresolvable <- other_placeholders[!other_placeholders %in%
                                        paste0("{", var_names, "}")]
    # Replace them with unique markers (using stringi for fixed replacement)
    markers <- paste0("__PLACEHOLDER_", seq_along(unresolvable), "__")
    for (i in seq_along(unresolvable)) {
      temp_format <- stringi::stri_replace_all_fixed(temp_format,
                                                     unresolvable[i],
                                                     markers[i])
    }
    # Apply glue
    result <- glue::glue_data(var_info, temp_format)
    # Restore original placeholders
    for (i in seq_along(markers)) {
      result <- stringi::stri_replace_all_fixed(result, markers[i], unresolvable[i])
    }
    result
  } else {
    format
  }
}
