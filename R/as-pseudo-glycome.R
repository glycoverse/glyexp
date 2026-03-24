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
#' # Create a simple glycoproteomics experiment
#' expr_mat <- matrix(1:12, nrow = 4)
#' colnames(expr_mat) <- c("S1", "S2", "S3")
#' rownames(expr_mat) <- c("GP1", "GP2", "GP3", "GP4")
#'
#' sample_info <- tibble::tibble(sample = c("S1", "S2", "S3"))
#' var_info <- tibble::tibble(
#'   variable = c("GP1", "GP2", "GP3", "GP4"),
#'   protein = c("P1", "P1", "P2", "P2"),
#'   protein_site = c(1L, 2L, 1L, 2L),
#'   glycan_composition = glyrepr::as_glycan_composition(c("H5N4", "H5N4", "H6N5", "H6N5"))
#' )
#'
#' gp_exp <- experiment(expr_mat, sample_info, var_info,
#'   exp_type = "glycoproteomics", glycan_type = "N"
#' )
#'
#' # Convert to pseudo-glycome
#' pseudo_glycome <- as_pseudo_glycome(gp_exp)
#' pseudo_glycome
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

  # Get unique groups
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
    # Aggregate expression matrix by summing within each group
    expr_mat_agg <- purrr::map_dfr(unique_groups, function(g) {
      rows <- which(groups == g)
      if (length(rows) == 1) {
        as.data.frame(t(exp$expr_mat[rows, , drop = FALSE]))
      } else {
        as.data.frame(t(colSums(
          exp$expr_mat[rows, , drop = FALSE],
          na.rm = TRUE
        )))
      }
    })
    expr_mat_agg <- as.matrix(expr_mat_agg)
    rownames(expr_mat_agg) <- seq_len(nrow(expr_mat_agg))
    colnames(expr_mat_agg) <- colnames(exp$expr_mat)
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
