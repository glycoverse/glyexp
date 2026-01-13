#' Compute <aa><pos> for non-NA protein_site values
#' @keywords internal
.compute_site_aa_pos_non_na <- function(var_info, fasta = NULL, taxid = 9606) {
  has_peptide <- "peptide" %in% colnames(var_info) && "peptide_site" %in% colnames(var_info)

  aa <- if (has_peptide) {
    .get_aa_from_peptide(var_info)
  } else if (!is.null(fasta)) {
    .get_aa_from_fasta(var_info, fasta)
  } else {
    .get_aa_from_uniprot(var_info, taxid)
  }

  # If peptide_site was NA, AA will be "" -> use "X"
  dplyr::if_else(aa == "", "X", paste0(aa, var_info$protein_site))
}
