#' Get amino acid from peptide sequence at peptide_site position
#' @keywords internal
.get_aa_from_peptide <- function(var_info) {
  purrr::map2_chr(
    var_info$peptide,
    var_info$peptide_site,
    ~ substr(.x, .y, .y)
  )
}
