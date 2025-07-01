#' Identification overview
#'
#' These functions are used to identify the number of 
#' glycan compositions, glycan structures, glycopeptides, peptides, 
#' glycoforms, glycoproteins, and glycosites in an [experiment()].
#'
#' @details
#' The following columns are required in the variable information tibble:
#' - `count_compositions()`: `glycan_composition`
#' - `count_structures()`: `glycan_structure`
#' - `count_peptides()`: `peptide`
#' - `count_glycopeptides()`: `glycan_composition` or `glycan_structure`, `peptide`, `peptide_site`
#' - `count_glycoforms()`: `glycan_composition` or `glycan_structure`,
#'   `protein` or `proteins`, `protein_site` or `protein_sites`
#' - `count_proteins()`: `protein` or `proteins`
#' - `count_glycosites()`: `protein` or `proteins`, `protein_site` or `protein_sites`
#'
#' You can use `count_struct` parameter to control how to count glycopeptides and glycoforms,
#' either by glycan structures or by glycan compositions.
#'
#' @param x An [experiment()] object.
#' @param count_struct For `count_glycopeptides()` and `count_glycoforms()`,
#' whether to count the number of glycan structures or glycopeptides.
#' If `TRUE`, glycopeptides or glycoforms bearing different glycan structures
#' with the same glycan composition are counted as different ones.
#' If not provided (NULL), defaults to `TRUE` if `glycan_structure` column exists
#' in the variable information tibble, otherwise `FALSE`.
#'
#' @return An integer.
#' - `count_compositions()`: The number of glycan compositions.
#' - `count_structures()`: The number of glycan structures.
#' - `count_peptides()`: The number of peptides.
#' - `count_glycopeptides()`: The number of unique combinations of peptides, sites, and glycans.
#' - `count_glycoforms()`: The number of unique combinations of proteins, sites, and glycans.
#' - `count_proteins()`: The number of proteins.
#' - `count_glycosites()`: The number of unique combinations of proteins and sites.
#'
#' @export
count_compositions <- function(x) {
  checkmate::assert_class(x, "glyexp_experiment")
  .assert_col_exists(x, "glycan_composition")
  dplyr::n_distinct(x$var_info[["glycan_composition"]])
}

#' @rdname count_compositions
#' @export
count_structures <- function(x) {
  checkmate::assert_class(x, "glyexp_experiment")
  .assert_col_exists(x, "glycan_structure")
  dplyr::n_distinct(x$var_info[["glycan_structure"]])
}

#' @rdname count_compositions
#' @export
count_peptides <- function(x) {
  checkmate::assert_class(x, "glyexp_experiment")
  .assert_exp_is_gp(x)
  .assert_col_exists(x, "peptide")
  dplyr::n_distinct(x$var_info[["peptide"]])
}

#' @rdname count_compositions
#' @export
count_glycopeptides <- function(x, count_struct = NULL) {
  checkmate::assert_class(x, "glyexp_experiment")
  .assert_exp_is_gp(x)
  checkmate::assert_flag(count_struct, null.ok = TRUE)

  has_struct <- "glycan_structure" %in% colnames(x$var_info)
  if (is.null(count_struct)) {
    count_struct <- has_struct
  }
  glycan_col <- if (count_struct) "glycan_structure" else "glycan_composition"

  .assert_col_exists(x, c("peptide", "peptide_site", glycan_col))
  dplyr::n_distinct(x$var_info[[glycan_col]], x$var_info[["peptide"]], x$var_info[["peptide_site"]])
}

#' @rdname count_compositions
#' @export
count_glycoforms <- function(x, count_struct = NULL) {
  checkmate::assert_class(x, "glyexp_experiment")
  .assert_exp_is_gp(x)
  checkmate::assert_flag(count_struct, null.ok = TRUE)

  has_struct <- "glycan_structure" %in% colnames(x$var_info)
  if (is.null(count_struct)) {
    count_struct <- has_struct
  }
  glycan_col <- if (count_struct) "glycan_structure" else "glycan_composition"

  pro_col <- .resolve_plural_col(x, "protein", "proteins")
  site_col <- .resolve_plural_col(x, "protein_site", "protein_sites")

  .assert_col_exists(x, c(glycan_col, pro_col, site_col))
  dplyr::n_distinct(x$var_info[[glycan_col]], x$var_info[[pro_col]], x$var_info[[site_col]])
}

#' @rdname count_compositions
#' @export
count_proteins <- function(x) {
  checkmate::assert_class(x, "glyexp_experiment")
  .assert_exp_is_gp(x)
  pro_col <- .resolve_plural_col(x, "protein", "proteins")
  .assert_col_exists(x, pro_col)
  dplyr::n_distinct(x$var_info[[pro_col]])
}

#' @rdname count_compositions
#' @export
count_glycosites <- function(x) {
  checkmate::assert_class(x, "glyexp_experiment")
  .assert_exp_is_gp(x)
  pro_col <- .resolve_plural_col(x, "protein", "proteins")
  site_col <- .resolve_plural_col(x, "protein_site", "protein_sites")
  .assert_col_exists(x, c(pro_col, site_col))
  dplyr::n_distinct(x$var_info[[pro_col]], x$var_info[[site_col]])
}

.assert_col_exists <- function(x, cols) {
  exist <- cols %in% colnames(x$var_info)
  if (!all(exist)) {
    missing_cols <- cols[!exist]
    cli::cli_abort(c("The following columns are missing in the variable information tibble: {missing_cols}."))
  }
}

.resolve_plural_col <- function(x, single, plural) {
  if (single %in% colnames(x$var_info)) {
    return(single)
  } else if (plural %in% colnames(x$var_info)) {
    return(plural)
  } else {
    cli::cli_abort("The variable information tibble must contain either {.field {single}} or {.field {plural}} column.")
  }
}

.assert_exp_is_gp <- function(x) {
  if (get_exp_type(x) != "glycoproteomics") {
    cli::cli_abort("The experiment type must be {.val glycoproteomics}.")
  }
}
