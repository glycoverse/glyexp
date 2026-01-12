#' Get amino acid from FASTA sequences at protein_site position
#' @keywords internal
.get_aa_from_fasta <- function(var_info, fasta) {
  # Handle character vector or file path
  if (is.character(fasta) && length(fasta) == 1 && file.exists(fasta)) {
    fasta <- seqinr::read.fasta(fasta, as.string = TRUE)
    seqs <- stats::setNames(vapply(fasta, function(x) x[1], ""), names(fasta))
  } else if (is.character(fasta)) {
    # Already a named character vector, use as-is
    seqs <- fasta
  } else {
    cli::cli_abort("{.arg fasta} must be a file path or named character vector.")
  }

  purrr::map2_chr(
    var_info$protein,
    var_info$protein_site,
    function(protein, site) {
      seq <- seqs[[protein]]
      if (is.null(seq)) {
        cli::cli_abort("Protein '{protein}' not found in fasta.")
      }
      substr(seq, site, site)
    }
  )
}
