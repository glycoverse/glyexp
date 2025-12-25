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
#' - `composition`: The number of glycan compositions.
#' - `structure`: The number of glycan structures.
#' - `peptide`: The number of peptides.
#' - `glycopeptide`: The number of unique combinations of peptides, sites, and glycans.
#' - `glycoform`: The number of unique combinations of proteins, sites, and glycans.
#' - `protein`: The number of proteins.
#' - `glycosite`: The number of unique combinations of proteins and sites.
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

  counts <- purrr::map_int(items, function(def) {
    if (def$gp && !is_gp) return(NA_integer_)
    if ("glycan_structure" %in% def$cols && !count_struct) return(NA_integer_)
    if (!all(def$cols %in% colnames(x$var_info))) return(NA_integer_)

    dplyr::n_distinct(x$var_info[, def$cols, drop = FALSE])
  })

  counts <- counts[!is.na(counts)]
  tibble::tibble(item = names(counts), n = unname(counts))
}
