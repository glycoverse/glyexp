#' Get amino acid from UniProt sequences
#' @keywords internal
.get_aa_from_uniprot <- function(var_info, taxid = 9606) {
  cli::cli_inform("Fetching protein sequences from UniProt (taxid: {taxid})...")

  unique_proteins <- unique(var_info$protein)

  # Fetch sequences using UniProt.ws
  seqs <- UniProt.ws::makeSingleStringQuery(unique_proteins) |>
    UniProt.ws::runQuery(taxid = taxid) |>
    UniProt.ws::getSequence()

  purrr::map2_chr(
    var_info$protein,
    var_info$protein_site,
    function(protein, site) {
      seq <- seqs[[protein]]
      if (is.null(seq)) {
        cli::cli_abort(
          c("Protein '{protein}' not found in UniProt.",
            i = "Try using format = '{protein}-{protein_site}-{{glycan_composition}}' directly.")
        )
      }
      substr(seq, site, site)
    }
  )
}
