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
#' - Expression values are summed within each glycan group
#'
#' **Limitation:** Glycopeptides can have different ionization efficiencies,
#' so this summing operation is not technically rigorous. Use results with caution.
#'
#' @param exp A glycoproteomics [experiment()].
#'
#' @return A glycomics-type [experiment()] with aggregated expression values.
#'   The `var_info` will contain only `glycan_composition` and `glycan_structure`
#'   (if present in input) columns.
#'
#' @export
#'
#' @examples
#' as_pseudo_glycome(real_experiment)
#'
as_pseudo_glycome <- function(exp) {
  # Validate input
  if (!is_experiment(exp)) {
    cli::cli_abort("{.arg exp} must be an experiment.")
  }

  exp_type <- get_exp_type(exp)
  if (exp_type != "glycoproteomics") {
    cli::cli_abort(c(
      "Input experiment must be a glycoproteomics experiment.",
      "x" = "Experiment type is {.val {exp_type}}.",
      "i" = "Use a glycoproteomics experiment created with {.code experiment(..., exp_type = 'glycoproteomics')}."
    ))
  }

  # Determine aggregation column
  if ("glycan_structure" %in% colnames(exp$var_info)) {
    agg_col <- "glycan_structure"
  } else {
    agg_col <- "glycan_composition"
  }

  # Get unique groups (in order of first appearance)
  groups <- exp$var_info[[agg_col]]
  unique_groups <- unique(groups)

  # Handle empty experiment case
  if (length(unique_groups) == 0) {
    expr_mat_agg <- matrix(
      nrow = 0,
      ncol = ncol(exp$expr_mat),
      dimnames = list(NULL, colnames(exp$expr_mat))
    )
  } else {
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
    # 4. Restore original order: After split(), use match() to reorder
    #    unique_groups to match the factor level order.

    group_keys <- as.character(groups)
    group_fac <- factor(group_keys, levels = unique(group_keys))

    # Aggregate expression matrix by summing within each group
    # Use split() on factor (much faster than which() in loop with glyrepr objects)
    row_groups <- split(seq_along(group_keys), group_fac)
    sample_names <- colnames(exp$expr_mat)

    # Use map_dfr to aggregate each group, ensuring consistent column structure
    expr_mat_agg <- purrr::map_dfr(row_groups, function(rows) {
      if (length(rows) == 1) {
        # Single row: just use it directly as a named vector
        result <- exp$expr_mat[rows, , drop = TRUE]
      } else {
        # Multiple rows: sum them
        result <- colSums(exp$expr_mat[rows, , drop = FALSE], na.rm = TRUE)
      }
      # Return as a one-row tibble with correct column names
      tibble::as_tibble_row(stats::setNames(as.list(result), sample_names))
    })
    expr_mat_agg <- as.matrix(expr_mat_agg)
    rownames(expr_mat_agg) <- seq_len(nrow(expr_mat_agg))

    # Reorder unique_groups to match the order of row_groups
    unique_groups <- unique_groups[match(
      names(row_groups),
      as.character(unique_groups)
    )]
  }

  # Build new var_info with only essential glycan columns
  var_info_new <- tibble::tibble(
    variable = as.character(seq_len(length(unique_groups)))
  )

  # Add aggregation column
  if (agg_col == "glycan_structure") {
    var_info_new$glycan_structure <- unique_groups
    var_info_new$glycan_composition <- glyrepr::as_glycan_composition(
      unique_groups
    )
  } else {
    var_info_new$glycan_composition <- unique_groups
  }

  # Create new experiment
  result <- experiment(
    expr_mat = expr_mat_agg,
    sample_info = exp$sample_info,
    var_info = var_info_new,
    exp_type = "glycomics",
    glycan_type = exp$meta_data$glycan_type
  )

  # Standardize variable IDs
  standardize_variable(result)
}
