#' Replace <site> token in format string with computed site values
#' @keywords internal
.resolve_site_token <- function(var_info, format, site_aa_pos) {
  if (stringr::str_detect(format, "<site>")) {
    if (length(site_aa_pos) != nrow(var_info)) {
      cli::cli_abort("Length of site_aa_pos must match number of variables.")
    }
    # Add site_aa_pos to var_info for glue resolution
    var_info$site_aa_pos <- site_aa_pos
    # Replace <site> token with {site_aa_pos} placeholder
    # The actual resolution happens in .glue_with_composition
    format <- stringr::str_replace_all(format, "<site>", "{site_aa_pos}")
    format
  } else {
    format
  }
}
