#' Convert a glycoproteomics experiment to a pseudo-glycome experiment
#'
#' @description
#' Transforms a glycoproteomics-type experiment into a glycomics-type experiment
#' by aggregating expression values by glycan structure (if available) or
#' glycan composition.
#'
#' This function implements the "pseudo-glycome" method described in
#' \doi{10.1038/s41467-026-68579-x}, which aggregates glycoproteomic data
#' by glycans to simulate glycome data when real glycome is unavailable.
#'
#' @details
#' **Aggregation behavior:**
#' - If `glycan_structure` column exists in `var_info`, aggregation is done
#'   by glycan structure (more specific)
#' - Otherwise, aggregation is done by `glycan_composition`
#' - Expression values are aggregated within each glycan group using the
#'   specified `aggr_method`
#'
#' **Limitation:** Glycopeptides can have different ionization efficiencies,
#' so the aggregation operation is not technically rigorous regardless of the
#' method used. Use results with caution.
#'
#' @param exp A glycoproteomics [experiment()] or a [GlycoproteomicSE()].
#' @param aggr_method Aggregation method to use. One of "sum", "mean", or
#'   "median". Default is "sum". Note that glycopeptides can have different
#'   ionization efficiencies, so none of these methods are technically rigorous.
#'
#' @return
#' If `exp` is an [experiment()], a glycomics-type [experiment()] with
#' aggregated expression values.
#'
#' If `exp` is a [GlycoproteomicSE()], a [GlycomicSE()] with aggregated
#' expression values.
#'
#' The variable metadata will contain only `glycan_composition` and
#' `glycan_structure` (if present in input) columns.
#'
#' @export
#'
#' @examples
#' library(glyrepr)
#' as_pseudo_glycome(real_experiment)
#'
as_pseudo_glycome <- function(exp, aggr_method = c("sum", "mean", "median")) {
  # Validate input
  is_glycoproteomic_se_input <- is_glycoproteomic_se(exp)
  is_experiment_input <- .is_experiment(exp)
  if (!is_experiment_input && !is_glycoproteomic_se_input) {
    cli::cli_abort("{.arg exp} must be an experiment or GlycoproteomicSE.")
  }

  # Validate and match aggregation method argument
  aggr_method <- rlang::arg_match(aggr_method)

  if (is_glycoproteomic_se_input) {
    return(.as_pseudo_glycome_glycoproteomic_se(exp, aggr_method))
  }

  .as_pseudo_glycome_experiment(exp, aggr_method)
}

#' Convert an experiment to pseudo-glycome
#'
#' @param exp A glycoproteomics [experiment()].
#' @param aggr_method Aggregation method to use.
#' @returns A glycomics [experiment()].
#' @noRd
.as_pseudo_glycome_experiment <- function(exp, aggr_method) {
  exp_type <- exp$meta_data$exp_type
  if (exp_type != "glycoproteomics") {
    cli::cli_abort(c(
      "Input experiment must be a glycoproteomics experiment.",
      "x" = "Experiment type is {.val {exp_type}}.",
      "i" = "Use a glycoproteomics experiment created with {.code experiment(..., exp_type = 'glycoproteomics')}."
    ))
  }

  pseudo_glycome <- .pseudo_glycome_data(
    expr_mat = exp$expr_mat,
    var_info = exp$var_info,
    aggr_method = aggr_method
  )

  # Create new experiment
  result <- .experiment(
    expr_mat = pseudo_glycome$expr_mat,
    sample_info = exp$sample_info,
    var_info = pseudo_glycome$var_info,
    exp_type = "glycomics",
    glycan_type = exp$meta_data$glycan_type
  )

  # Standardize variable IDs
  standardize_variable(result)
}

#' Convert a GlycoproteomicSE to pseudo-glycome
#'
#' @param exp A [GlycoproteomicSE()].
#' @param aggr_method Aggregation method to use.
#' @returns A [GlycomicSE()].
#' @noRd
.as_pseudo_glycome_glycoproteomic_se <- function(exp, aggr_method) {
  .require_se()

  pseudo_glycome <- .pseudo_glycome_data(
    expr_mat = SummarizedExperiment::assay(exp, 1),
    var_info = SummarizedExperiment::rowData(exp),
    aggr_method = aggr_method
  )
  meta_data <- S4Vectors::metadata(exp)
  meta_data$exp_type <- "glycomics"

  GlycomicSE(
    abundance = pseudo_glycome$expr_mat,
    colData = SummarizedExperiment::colData(exp),
    rowData = .pseudo_glycome_row_data(pseudo_glycome$var_info),
    metadata = meta_data
  )
}

#' Build pseudo-glycome abundance and variable metadata
#'
#' @param expr_mat A numeric abundance matrix.
#' @param var_info Variable metadata with glycan columns.
#' @param aggr_method Aggregation method to use.
#' @returns A list with `expr_mat` and `var_info`.
#' @noRd
.pseudo_glycome_data <- function(expr_mat, var_info, aggr_method) {
  # Determine aggregation column
  if ("glycan_structure" %in% colnames(var_info)) {
    agg_col <- "glycan_structure"
  } else {
    agg_col <- "glycan_composition"
  }

  # Get unique groups (in order of first appearance)
  groups <- var_info[[agg_col]]
  unique_groups <- unique(groups)
  expr_mat_agg <- .aggregate_pseudo_glycome_expr_mat(
    expr_mat = expr_mat,
    groups = groups,
    unique_groups = unique_groups,
    aggr_method = aggr_method
  )

  list(
    expr_mat = expr_mat_agg,
    var_info = .pseudo_glycome_var_info(
      unique_groups = unique_groups,
      agg_col = agg_col
    )
  )
}

#' Aggregate an expression matrix by glycan group
#'
#' @param expr_mat A numeric abundance matrix.
#' @param groups Glycan groups, one per matrix row.
#' @param unique_groups Unique glycan groups.
#' @param aggr_method Aggregation method to use.
#' @returns An aggregated abundance matrix.
#' @noRd
.aggregate_pseudo_glycome_expr_mat <- function(
  expr_mat,
  groups,
  unique_groups,
  aggr_method
) {
  # Handle empty experiment case
  if (length(unique_groups) == 0) {
    return(matrix(
      nrow = 0,
      ncol = ncol(expr_mat),
      dimnames = list(NULL, colnames(expr_mat))
    ))
  }

  # Performance optimization for grouping:
  #
  # The naive approach would be to use `which(groups == g)` in a loop over
  # unique_groups. However, this is extremely slow when `groups` is a
  # glyrepr_structure or glyrepr_composition column because each `==` comparison
  # involves expensive complex object comparisons (igraph graphs or named vectors).
  #
  # For a dataset like real_experiment (4262 variables), this would trigger
  # ~4262^2/2 expensive comparisons, making the function unusably slow.
  #
  # Solution:
  # 1. Convert to character keys: `as.character(groups)` extracts string
  #    representations (e.g., "Hex(5)HexNAc(4)" or IUPAC structure strings).
  #    String comparison is O(1) and much faster than object comparison.
  #
  # 2. Use factor with custom levels: `factor(..., levels = unique(group_keys))`
  #    preserves the order of first appearance. Without this, split() would
  #    sort the groups alphabetically, changing the result order.
  #
  # 3. Use split() on the factor: This groups row indices by their character
  #    keys in a single pass, avoiding the O(n^2) loop.
  #
  # 4. The variable metadata is built from `unique_groups`, which preserves
  #    the same first-appearance order used by the factor levels.

  group_keys <- as.character(groups)
  group_fac <- factor(group_keys, levels = unique(group_keys))
  row_groups <- split(seq_along(group_keys), group_fac)
  expr_rows <- purrr::map(row_groups, function(rows) {
    mat_subset <- expr_mat[rows, , drop = FALSE]
    .aggregate_pseudo_glycome_rows(mat_subset, aggr_method)
  })
  expr_mat_agg <- do.call(rbind, expr_rows)
  colnames(expr_mat_agg) <- colnames(expr_mat)
  rownames(expr_mat_agg) <- seq_len(nrow(expr_mat_agg))
  expr_mat_agg
}

#' Aggregate rows for one pseudo-glycome group
#'
#' @param mat_subset A matrix containing one glycan group.
#' @param aggr_method Aggregation method to use.
#' @returns A named numeric vector with one value per sample.
#' @noRd
.aggregate_pseudo_glycome_rows <- function(mat_subset, aggr_method) {
  if (nrow(mat_subset) == 1) {
    return(mat_subset[1, , drop = TRUE])
  }

  # Columns where all values are NA should return NA, not 0 or NaN.
  all_na_cols <- purrr::map_lgl(seq_len(ncol(mat_subset)), function(col) {
    all(is.na(mat_subset[, col]))
  })

  result <- switch(
    aggr_method,
    sum = colSums(mat_subset, na.rm = TRUE),
    mean = colMeans(mat_subset, na.rm = TRUE),
    median = purrr::map_dbl(seq_len(ncol(mat_subset)), function(col) {
      stats::median(mat_subset[, col], na.rm = TRUE)
    })
  )
  names(result) <- colnames(mat_subset)

  # For columns where all values were NA, force result to NA.
  result[all_na_cols] <- NA_real_
  result
}

#' Build pseudo-glycome variable metadata
#'
#' @param unique_groups Unique glycan groups.
#' @param agg_col Aggregation column name.
#' @returns A tibble with pseudo-glycome variable metadata.
#' @noRd
.pseudo_glycome_var_info <- function(unique_groups, agg_col) {
  var_info <- tibble::tibble(
    variable = as.character(seq_len(length(unique_groups)))
  )

  # Add aggregation column
  if (agg_col == "glycan_structure") {
    var_info$glycan_structure <- unique_groups
    var_info$glycan_composition <- glyrepr::as_glycan_composition(
      unique_groups
    )
  } else {
    var_info$glycan_composition <- unique_groups
  }

  var_info
}

#' Convert pseudo-glycome variable metadata to rowData
#'
#' @param var_info Pseudo-glycome variable metadata.
#' @returns An `S4Vectors::DataFrame()`.
#' @noRd
.pseudo_glycome_row_data <- function(var_info) {
  row_names <- var_info$variable
  row_data <- var_info[setdiff(colnames(var_info), "variable")]
  S4Vectors::DataFrame(row_data, row.names = row_names)
}
