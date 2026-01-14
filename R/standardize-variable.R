#' Standardize variable IDs in an experiment
#'
#' @description
#' Converts meaningless variable IDs (like "GP1", "V1") into meaningful,
#' human-readable IDs based on the experiment type and available columns.
#'
#' The format of the new IDs depends on the `exp_type` if `format` is not specified:
#' - `glycomics`: `{glycan_composition}`, e.g., "Hex(5)HexNAc(2)"
#' - `glycoproteomics`: `{protein}-<site>-{glycan_composition}` or
#'   `{protein}-{glycan_composition}` if no `protein_site` column exists
#' - `traitomics`: `{motif}` or `{trait}` depending on which column is present
#' - `traitproteomics`: `{protein}-<site>-{motif}` or `{protein}-<site>-{trait}`
#'
#' If duplicate IDs are generated (e.g., same composition with multiple PSMs),
#' a unique integer suffix is appended using the `unique_suffix` pattern.
#'
#' @param exp An [experiment()].
#' @param format A format string specifying how to construct variable IDs.
#'   Use `{column_name}` to insert values from `var_info` columns.
#'   For example, `"{gene}-{glycan_composition}"` would produce "GENE1-Hex(5)".
#'   If `NULL` (default), a sensible format is chosen based on `exp_type`.
#'   Use `<site>` to include the amino acid and position (e.g., "N32").
#'   The `<site>` token is a special placeholder that gets replaced with
#'   `<aa><pos>` format (e.g., "N32", "S44"). It requires the `protein_site`
#'   column and uses the following decision tree to determine the amino acid:
#'   1. If `peptide` and `peptide_site` columns exist, extract from peptide
#'   2. Else if `fasta` is provided, extract from FASTA sequences
#'   3. Else fetch from UniProt using `UniProt.ws`
#' @param fasta Either a file path to a FASTA file or a named character vector
#'   with protein IDs as names and sequences as values. Used to look up amino
#'   acids for site representation when peptide columns are not available.
#'   Default: `NULL` (use UniProt.ws to fetch sequences).
#' @param taxid NCBI taxonomy ID for UniProt lookup. Default: `9606` (human).
#' @param unique_suffix A string pattern for making IDs unique when duplicates exist.
#'   Must contain `{N}` which will be replaced with the numeric suffix (1, 2, 3...).
#'   Default is `"-{N}"` which produces IDs like "Hex(5)-1", "Hex(5)-2".
#'
#' @return The experiment with standardized variable IDs, invisibly.
#'
#' @examples
#' \dontrun{
#' # Glycomics example
#' \dontrun{
#' expr_mat <- matrix(1:4, nrow = 2)
#' rownames(expr_mat) <- c("V1", "V2")
#' colnames(expr_mat) <- c("S1", "S2")
#' sample_info <- tibble::tibble(sample = c("S1", "S2"))
#' var_info <- tibble::tibble(
#'   variable = c("V1", "V2"),
#'   glycan_composition = glyrepr::glycan_composition(c(Hex = 5, HexNAc = 2))
#' )
#' exp <- experiment(expr_mat, sample_info, var_info,
#'   exp_type = "glycomics", glycan_type = "N"
#' )
#' standardize_variable(exp)
#' }
#'
#' # Glycoproteomics example
#' \dontrun{
#' expr_mat <- matrix(1:4, nrow = 2)
#' rownames(expr_mat) <- c("GP1", "GP2")
#' colnames(expr_mat) <- c("S1", "S2")
#' sample_info <- tibble::tibble(sample = c("S1", "S2"))
#' var_info <- tibble::tibble(
#'   variable = c("GP1", "GP2"),
#'   protein = c("P12345", "P12345"),
#'   protein_site = c(32L, 45L),
#'   peptide = c("NKT", "LPNG"),
#'   peptide_site = c(1L, 2L),
#'   glycan_composition = glyrepr::glycan_composition(c(Hex = 5, HexNAc = 2))
#' )
#' exp <- experiment(expr_mat, sample_info, var_info,
#'   exp_type = "glycoproteomics", glycan_type = "N"
#' )
#' standardize_variable(exp)
#' }
#'
#' # FASTA example
#' fasta <- c(P12345 = "MABCDEFGHIJKLMNOPQRSTUVWXYZ")
#' standardize_variable(exp, fasta = fasta)
#'
#' # Custom format with <site> token
#' standardize_variable(exp, format = "{protein}-<site>-{glycan_composition}")
#' }
#'
#' # Custom format example
#' \dontrun{
#' standardize_variable(exp, format = "{protein}-{glycan_composition}")
#' }
#'
#' @export
standardize_variable <- function(exp, format = NULL, unique_suffix = "-{N}",
                                  fasta = NULL, taxid = 9606) {
  if (!is_experiment(exp)) {
    cli::cli_abort("{.arg exp} must be an experiment.")
  }

  # Validate unique_suffix contains {N}
  if (!stringr::str_detect(unique_suffix, "\\{N\\}")) {
    cli::cli_abort("{.arg unique_suffix} must contain {.val {N}} as a placeholder.")
  }

  exp_type <- exp$meta_data$exp_type
  var_info <- exp$var_info

  # Determine format if not provided
  if (is.null(format)) {
    format <- .get_default_format(exp_type, var_info)
  }

  # Compute <site> token if present
  site_aa_pos <- NULL
  if (stringr::str_detect(format, "<site>")) {
    if (!"protein_site" %in% colnames(var_info)) {
      cli::cli_abort("<site> token requires protein_site column.")
    }
    site_aa_pos <- .compute_site_aa_pos(var_info, fasta = fasta, taxid = taxid)
    # Add site_aa_pos to var_info for glue resolution
    var_info$site_aa_pos <- site_aa_pos
  }

  # Resolve format (replacing <site> if present)
  format <- .resolve_site_token(var_info, format, site_aa_pos)

  # Generate new variable IDs using glue
  new_vars <- .glue_with_composition(var_info, format)

  # Ensure uniqueness by adding suffix if needed
  new_vars <- .ensure_unique(new_vars, unique_suffix)

  # Update var_info and expr_mat
  exp$var_info$variable <- new_vars
  rownames(exp$expr_mat) <- new_vars

  invisible(exp)
}

#' Format variable strings with special handling for glycan_composition
#' @keywords internal
.glue_with_composition <- function(var_info, format) {
  # Check if format contains glycan_composition and it's a list column
  if (stringr::str_detect(format, "\\{glycan_composition\\}")) {
    if ("glycan_composition" %in% colnames(var_info) && glyrepr::is_glycan_composition(var_info$glycan_composition)) {
      var_info$glycan_composition_char <- as.character(var_info$glycan_composition)
      format <- stringr::str_replace_all(format, "\\{glycan_composition\\}", "{glycan_composition_char}")
    }
  }
  glue::glue_data(var_info, format)
}

#' Get default format string based on exp_type
#' @keywords internal
.get_default_format <- function(exp_type, var_info) {
  switch(exp_type,
    "glycomics" = {
      if (!"glycan_composition" %in% colnames(var_info)) {
        cli::cli_abort("glycan_composition column is required for glycomics experiments.")
      }
      "{glycan_composition}"
    },
    "glycoproteomics" = {
      if (!"glycan_composition" %in% colnames(var_info)) {
        cli::cli_abort("glycan_composition column is required for glycoproteomics experiments.")
      }
      if ("protein_site" %in% colnames(var_info)) {
        "{protein}-<site>-{glycan_composition}"
      } else {
        "{protein}-{glycan_composition}"
      }
    },
    "traitomics" = {
      if ("motif" %in% colnames(var_info) && !all(is.na(var_info$motif))) {
        "{motif}"
      } else if ("trait" %in% colnames(var_info)) {
        "{trait}"
      } else {
        cli::cli_abort("Either 'motif' or 'trait' column is required for traitomics experiments.")
      }
    },
    "traitproteomics" = {
      if (!"protein_site" %in% colnames(var_info)) {
        cli::cli_abort("protein_site column is required for traitproteomics experiments.")
      }
      if ("motif" %in% colnames(var_info) && !all(is.na(var_info$motif))) {
        "{protein}-<site>-{motif}"
      } else if ("trait" %in% colnames(var_info)) {
        "{protein}-<site>-{trait}"
      } else {
        cli::cli_abort("Either 'motif' or 'trait' column is required for traitproteomics experiments.")
      }
    },
    cli::cli_abort("exp_type '{exp_type}' is not supported.")
  )
}

#' Ensure variable IDs are unique by adding numeric suffixes
#' @keywords internal
.ensure_unique <- function(vars, unique_suffix) {
  if (length(vars) == 0) return(vars)

  if (length(unique(vars)) == length(vars)) {
    return(vars)
  }

  # Count occurrences of each value
  var_counts <- table(vars)

  # Track current count for each unique value
  current_counts <- stats::setNames(rep(0, length(var_counts)), names(var_counts))
  result <- character(length(vars))

  for (i in seq_along(vars)) {
    v <- vars[[i]]
    if (var_counts[[v]] > 1) {
      current_counts[[v]] <- current_counts[[v]] + 1
      suffix <- stringr::str_replace(unique_suffix, "\\{N\\}", as.character(current_counts[[v]]))
      result[[i]] <- paste0(v, suffix)
    } else {
      result[[i]] <- v
    }
  }

  result
}

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

#' Get amino acid from peptide sequence at peptide_site position
#' @keywords internal
.get_aa_from_peptide <- function(var_info) {
  purrr::map2_chr(
    var_info$peptide,
    var_info$peptide_site,
    ~ substr(.x, .y, .y)
  )
}

#' Get amino acid from UniProt sequences
#' @keywords internal
.get_aa_from_uniprot <- function(var_info, taxid = 9606) {
  cli::cli_inform("Fetching protein sequences from UniProt (taxid: {taxid})...")

  unique_proteins <- unique(var_info$protein)

  # Fetch sequences using UniProt API
  # Taxonomy filter is included in the query string
  query_str <- paste(unique_proteins, collapse = " OR ")
  full_query <- paste0("organism_id:", taxid, " AND (", query_str, ")")

  result <- UniProt.ws::queryUniProt(
    query = full_query,
    fields = c("accession", "sequence")
  )

  # Convert to named character vector
  # Column names from UniProt.ws are "Entry" and "Sequence"
  seqs <- stats::setNames(result$Sequence, result$Entry)

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
