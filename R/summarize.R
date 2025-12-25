#' Identification overview
#'
#' This function summarizes the number of
#' glycan compositions, glycan structures, glycopeptides, peptides,
#' glycoforms, glycoproteins, and glycosites in an [experiment()].
#'
#' @details
#' The following columns are required in the variable information tibble:
#' - `composition`: `glycan_composition`
#' - `structure`: `glycan_structure`
#' - `peptide`: `peptide`
#' - `glycopeptide`: `glycan_composition` or `glycan_structure`, `peptide`, `peptide_site`
#' - `glycoform`: `glycan_composition` or `glycan_structure`,
#'   `protein` or `proteins`, `protein_site` or `protein_sites`
#' - `protein`: `protein` or `proteins`
#' - `glycosite`: `protein` or `proteins`, `protein_site` or `protein_sites`
#'
#' You can use `count_struct` parameter to control how to count glycopeptides and glycoforms,
#' either by glycan structures or by glycan compositions.
#'
#' @param x An [experiment()] object.
#' @param count_struct For counting glycopeptides and glycoforms.
#' whether to count the number of glycan structures or glycopeptides.
#' If `TRUE`, glycopeptides or glycoforms bearing different glycan structures
#' with the same glycan composition are counted as different ones.
#' If not provided (NULL), defaults to `TRUE` if `glycan_structure` column exists
#' in the variable information tibble, otherwise `FALSE`.
#'
#' @importFrom tibble tibble
#'
#' @return A tibble with columns `item` and `n`
#'   summarizing the results. The items include:
#' - `total_composition`: The number of glycan compositions.
#' - `total_structure`: The number of glycan structures.
#' - `total_peptide`: The number of peptides.
#' - `total_glycopeptide`: The number of unique combinations of peptides, sites, and glycans.
#' - `total_glycoform`: The number of unique combinations of proteins, sites, and glycans.
#' - `total_protein`: The number of proteins.
#' - `total_glycosite`: The number of unique combinations of proteins and sites.
#'
#' In addition, `_per_sample` items are calculated as the average number of detected items per sample.
#' For example, `composition_per_sample` is the average number of glycan compositions detected per sample.
#'
#' @examples
#' exp <- real_experiment
#' summarize_experiment(exp)
#'
#' @export
summarize_experiment <- function(x, count_struct = NULL) {
  checkmate::assert_class(x, "glyexp_experiment")
  checkmate::assert_flag(count_struct, null.ok = TRUE)

  has_struct <- "glycan_structure" %in% colnames(x$var_info)
  if (is.null(count_struct)) {
    count_struct <- has_struct
  } else if (count_struct && !has_struct) {
    cli::cli_abort(c(
      "Column {.field glycan_structure} is not found in the variable information tibble.",
      "i" = "Please set {.arg count_struct} to {.val FALSE}."
    ))
  }

  glycan_col <- if (count_struct) "glycan_structure" else "glycan_composition"
  is_gp <- get_exp_type(x) == "glycoproteomics"

  items <- list(
    composition = list(cols = "glycan_composition", gp = FALSE),
    structure = list(cols = "glycan_structure", gp = FALSE),
    peptide = list(cols = "peptide", gp = TRUE),
    glycopeptide = list(cols = c(glycan_col, "peptide", "peptide_site"), gp = TRUE),
    glycoform = list(cols = c(glycan_col, "protein", "protein_site"), gp = TRUE),
    protein = list(cols = "protein", gp = TRUE),
    glycosite = list(cols = c("protein", "protein_site"), gp = TRUE)
  )

  # Calculate total counts
  total_counts <- purrr::map(items, function(def) {
    if (def$gp && !is_gp) return(NULL)
    if ("glycan_structure" %in% def$cols && !count_struct) return(NULL)
    if (!all(def$cols %in% colnames(x$var_info))) return(NULL)

    dplyr::n_distinct(x$var_info[, def$cols, drop = FALSE])
  })
  names(total_counts) <- paste0("total_", names(total_counts))

  # Calculate per-sample counts
  per_sample_counts <- purrr::map(items, function(def) {
    if (def$gp && !is_gp) return(NULL)
    if ("glycan_structure" %in% def$cols && !count_struct) return(NULL)
    if (!all(def$cols %in% colnames(x$var_info))) return(NULL)

    # For each sample, count unique items with non-NA values
    sample_counts <- purrr::map_int(colnames(x$expr_mat), function(s) {
      # Get variables detected in this sample
      detected_vars <- rownames(x$expr_mat)[!is.na(x$expr_mat[, s])]
      if (length(detected_vars) == 0) return(0L)

      # Filter var_info for these variables
      var_subset <- x$var_info[match(detected_vars, x$var_info$variable), , drop = FALSE]

      # Count distinct
      dplyr::n_distinct(var_subset[, def$cols, drop = FALSE])
    })
    mean(sample_counts)
  })
  names(per_sample_counts) <- paste0(names(per_sample_counts), "_per_sample")

  # Combine and format
  all_counts <- c(total_counts, per_sample_counts)
  all_counts <- all_counts[!vapply(all_counts, is.null, logical(1))]

  tibble::tibble(item = names(all_counts), n = as.numeric(unlist(all_counts)))
}
