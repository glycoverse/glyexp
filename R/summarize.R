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

  if (is.null(count_struct)) {
    count_struct <- "glycan_structure" %in% colnames(x$var_info)
  } else if (count_struct) {
    if (!("glycan_structure" %in% colnames(x$var_info))) {
      cli::cli_abort(c(
        "Column {.field glycan_structure} is not found in the variable information tibble.",
        "i" = "Please set {.arg count_struct} to {.val FALSE}."
      ))
    }
  } else {
    x$var_info$glycan_structure <- NULL
  }
  funcs <- list(
    composition = .count_compositions,
    structure = .count_structures,
    peptide = .count_peptides,
    glycopeptide = .count_glycopeptides,
    glycoform = .count_glycoforms,
    protein = .count_proteins,
    glycosite = .count_glycosites
  )
  funcs <- purrr::map(funcs, purrr::safely, otherwise = NA)
  counts <- purrr::map_int(funcs, ~ .x(x)$result)
  counts <- counts[!is.na(counts)]
  tibble::tibble(item = names(counts), n = unname(counts))
}

.count_compositions <- function(x) {
  .count_distinct(x, "glycan_composition")
}

.count_structures <- function(x) {
  .count_distinct(x, "glycan_structure")
}

.count_peptides <- function(x) {
  .count_distinct(x, "peptide", .needs_gp = TRUE)
}

.count_glycopeptides <- function(x, count_struct = NULL) {
  glycan_col <- .resolve_glycan_col(x, count_struct)
  .count_distinct(x, glycan_col, "peptide", "peptide_site", .needs_gp = TRUE)
}

.count_glycoforms <- function(x, count_struct = NULL) {
  glycan_col <- .resolve_glycan_col(x, count_struct)
  .count_distinct(x, glycan_col, "protein", "protein_site", .needs_gp = TRUE)
}

.count_proteins <- function(x) {
  .count_distinct(x, "protein", .needs_gp = TRUE)
}

.count_glycosites <- function(x) {
  .count_distinct(x, "protein", "protein_site", .needs_gp = TRUE)
}

.count_distinct <- function(x, ..., .needs_gp = FALSE) {
  checkmate::assert_class(x, "glyexp_experiment")
  if (.needs_gp) {
    .assert_exp_is_gp(x)
  }

  cols <- c(...)
  .assert_col_exists(x, cols)

  dplyr::n_distinct(x$var_info[, cols, drop = FALSE])
}

.resolve_glycan_col <- function(x, count_struct) {
  checkmate::assert_flag(count_struct, null.ok = TRUE)
  has_struct <- "glycan_structure" %in% colnames(x$var_info)
  if (is.null(count_struct)) {
    count_struct <- has_struct
  }
  if (count_struct) "glycan_structure" else "glycan_composition"
}

.assert_col_exists <- function(x, cols) {
  exist <- cols %in% colnames(x$var_info)
  if (!all(exist)) {
    cli::cli_abort(c(
      "The following columns are missing in the variable information tibble: {cols[!exist]}."
    ))
  }
}

.assert_exp_is_gp <- function(x) {
  if (get_exp_type(x) != "glycoproteomics") {
    cli::cli_abort("The experiment type must be {.val glycoproteomics}.")
  }
}
