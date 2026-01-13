#' Compute <aa><pos> site representation
#'
#' This is the main function implementing the decision tree for computing
#' the amino acid and position representation.
#'
#' @param var_info A tibble with protein, protein_site, and optionally
#'   peptide and peptide_site columns.
#' @param fasta Optional named character vector of protein sequences.
#' @param taxid UniProt taxonomy ID (default: 9606 for human).
#' @return A character vector of <aa><pos> site representations.
#' @keywords internal
.compute_site_aa_pos <- function(var_info, fasta = NULL, taxid = 9606) {
  dplyr::if_else(
    is.na(var_info$protein_site),
    "X",
    .compute_site_aa_pos_non_na(var_info, fasta, taxid)
  )
}
